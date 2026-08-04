module El = Brr.El
module At = Brr.At
module Ev = Brr.Ev

let pi = 4.0 *. atan 1.0
let now_ms () = Brr.(Performance.now_ms G.performance)

type region = {
  name : string;
  min_lat : float;
  max_lat : float;
  min_lon : float;
  max_lon : float;
  view_lat : float;
  view_lon : float;
}

(* Web Mercator (EPSG:3857), the "PM" tile matrix set of the service: 256 pixel
   tiles, origin at the top left, 2^level tiles per axis -- the ordinary slippy
   grid, so TILEROW and TILECOL are the usual y and x. This is the only place in
   the program that leaves the plate carree arcsecond grid the renderer works
   in, so the conversions stay local to this module. *)

let tile_px = 256

(* The relief layer stops at 18 while the basemap goes to 19. Capping both here
   keeps one tile level driving the two layers, which is what lets them be
   positioned by a single transform. *)
let max_level = 18
let world_px level = float tile_px *. Float.pow 2. (float level)
let lon_to_px ~level lon = (lon +. 180.) /. 360. *. world_px level

let lat_to_px ~level lat =
  (0.5 -. (Float.asinh (tan (lat *. pi /. 180.)) /. (2. *. pi)))
  *. world_px level

let px_to_lon ~level x = (x /. world_px level *. 360.) -. 180.

let px_to_lat ~level y =
  180. /. pi *. atan (Float.sinh ((0.5 -. (y /. world_px level)) *. 2. *. pi))

type layer = { id : string; tms : string }

let base_layer = { id = "GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2"; tms = "PM_0_19" }

(* Bare earth (MNT), not the surface model: buildings and tree canopy shaded in
   would fight the basemap's own rendering of them. LiDAR HD is still being
   flown, so outside the surveyed areas this answers a transparent tile rather
   than a 404 -- which under a multiply blend simply leaves the basemap be. *)
let relief_layer =
  {
    id = "IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE.SHADOW";
    tms = "PM_0_18";
  }

let tile_url l ~level ~row ~col =
  Printf.sprintf
    "https://data.geopf.fr/wmts?SERVICE=WMTS&VERSION=1.0.0&REQUEST=GetTile&LAYER=%s&STYLE=normal&FORMAT=image/png&TILEMATRIXSET=%s&TILEMATRIX=%d&TILEROW=%d&TILECOL=%d"
    l.id l.tms level row col

(* [Ev.error] is typed for the global error event, whose payload is of no use
   here: an image failure only ever needs to be noticed. *)
let img_error_ev : Ev.void = Ev.Type.create (Jstr.v "error")

(* Time constant of the drag inertia, in milliseconds: the same rate of decay as
   the terrain viewer's 0.95 per 16.6 ms, written as a time constant because the
   glide integrates it rather than applying it per frame. A fling therefore
   travels its release speed times this, about a third of a second's worth. *)
let glide_tau = -16.6 /. log 0.95
let px v = Jstr.v (Printf.sprintf "%.2fpx" v)
let clampf lo hi v = Float.min (Float.max v lo) hi
let clampi lo hi v = Int.min (Int.max v lo) hi

let svg_button cls title inner =
  let el = El.button ~at:At.[ class' (Jstr.v cls) ] [] in
  Jv.set (El.to_jv el) "innerHTML" (Jv.of_string inner);
  El.set_at (Jstr.v "title") (Some (Jstr.v title)) el;
  el

(* One container of tiles per zoom level, each with its own transform, so the
   level being left behind can stay on screen underneath the one coming in until
   that one has no gaps left. Without it a zoom step blanks the map back to the
   background for as long as a screen of tiles takes to arrive.

   [ox] and [oy] are the origin the tiles' [left]/[top] are measured from, which
   the transform has to undo; they belong to the level because a retained level
   keeps the origin it was last laid out with while the live one moves on. *)
type tile_level = {
  container : El.t;
  tiles : (int * int, El.t) Hashtbl.t;
  mutable ox : float;
  mutable oy : float;
}

let create ~regions ~in_range ~traces ~on_select =
  let first_region =
    match regions with
    | r :: _ -> r
    | [] -> invalid_arg "Map_picker.create: no region"
  in
  let region = ref first_region in
  let center_lat = ref 0. and center_lon = ref 0. in
  let zoom = ref 13. in

  (* Where each region was last left. Switching away and back returns to it, and
     so does closing and reopening the map, which is the whole point: having
     zoomed in to pick between two cols, coming back to the same place at the
     same scale is what one expects, not starting over at the region's default.

     Seeded with the region's own view point. Recorded on every render rather
     than at the exits, so leaving by any route -- a region switch, cancel, a
     confirmation, a key -- finds the view already written down. *)
  let saved =
    List.map (fun r -> (r, ref (r.view_lat, r.view_lon, !zoom))) regions
  in
  let saved_of r =
    (* Physical equality: these are the very records the caller gave, and the
       ones the region buttons hold. *)
    match List.assq_opt r saved with
    | Some slot -> slot
    | None -> ref (r.view_lat, r.view_lon, !zoom)
  in

  (* Drag inertia, in screen pixels per millisecond. [glide] is a generation: a
     new gesture bumps it and the running animation sees it no longer owns the
     map and gives up, which is cheaper to get right than cancelling a frame
     request. *)
  let vel_x = ref 0. and vel_y = ref 0. in
  let last_move_ms = ref 0. in
  let glide = ref 0 in
  let stop_glide () =
    incr glide;
    vel_x := 0.;
    vel_y := 0.
  in

  (* Focusable so the map can take the focus off whatever opened it, keeping
     typing out of controls left behind it; [restore_focus] hands it back. *)
  let overlay =
    El.div ~at:At.[ class' (Jstr.v "map-overlay"); tabindex (-1) ] []
  in
  let restore_focus = ref None in

  (* Layout: one layer of tiles, each holding its own basemap and relief image,
     so the relief multiplies onto the tile under it and nothing else (see the
     isolation notes in index.html). The bounds rectangle, crosshair and controls
     come after the layer and are therefore never part of that blend. *)
  let viewport = El.div ~at:At.[ class' (Jstr.v "map-viewport") ] [] in
  let layer_el = El.div ~at:At.[ class' (Jstr.v "map-layer") ] [] in
  (* The tracks, over the tiles but under the bounds rectangle, so the part of a
     trace outside the selectable area is dimmed along with the map beneath it.
     Built from markup because [El.v] calls [createElement], and SVG needs
     [createElementNS]; the group's transform is set per render like a tile
     level's, and the root clips to the viewport, which is exactly the part worth
     drawing. *)
  let trace_host = El.div ~at:At.[ class' (Jstr.v "map-trace") ] [] in
  Jv.set (El.to_jv trace_host) "innerHTML"
    (Jv.of_string
       {|<svg width="100%" height="100%"><g><path class="map-trace-casing" fill="none" vector-effect="non-scaling-stroke"/><path class="map-trace-line" fill="none" vector-effect="non-scaling-stroke"/></g></svg>|});
  let query sel =
    Jv.call (El.to_jv trace_host) "querySelector" [| Jv.of_string sel |]
  in
  let trace_group = query "g" in
  let trace_shapes = [ query ".map-trace-casing"; query ".map-trace-line" ] in
  let set_attr node name v =
    ignore (Jv.call node "setAttribute" [| Jv.of_string name; Jv.of_string v |])
  in

  let bounds = El.div ~at:At.[ class' (Jstr.v "map-bounds") ] [] in
  let crosshair = El.div ~at:At.[ class' (Jstr.v "map-crosshair") ] [] in
  let attribution =
    El.div
      ~at:At.[ class' (Jstr.v "map-attribution") ]
      [ El.txt (Jstr.v "\u{00A9} IGN \u{2014} Plan IGN, Relief du terrain HD") ]
  in
  let zoom_in =
    svg_button "map-zoom-btn" "Zoom in"
      {|<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><line x1="12" y1="5" x2="12" y2="19"/><line x1="5" y1="12" x2="19" y2="12"/></svg>|}
  in
  let zoom_out =
    svg_button "map-zoom-btn" "Zoom out"
      {|<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><line x1="5" y1="12" x2="19" y2="12"/></svg>|}
  in
  let zoom_box =
    El.div ~at:At.[ class' (Jstr.v "map-zoom") ] [ zoom_in; zoom_out ]
  in
  let coord = El.div ~at:At.[ class' (Jstr.v "map-coord") ] [] in
  let error_msg = El.p ~at:At.[ class' (Jstr.v "map-error") ] [] in
  let btn_cancel =
    El.button ~at:At.[ class' (Jstr.v "map-btn") ] [ El.txt (Jstr.v "Cancel") ]
  in
  let btn_ok =
    El.button
      ~at:At.[ class' (Jstr.v "map-btn"); class' (Jstr.v "map-btn-primary") ]
      [ El.txt (Jstr.v "Use this location") ]
  in
  let region_row = El.div ~at:At.[ class' (Jstr.v "map-regions") ] [] in
  let bar =
    El.div
      ~at:At.[ class' (Jstr.v "map-bar") ]
      [
        region_row;
        coord;
        error_msg;
        El.div ~at:At.[ class' (Jstr.v "map-actions") ] [ btn_cancel; btn_ok ];
      ]
  in
  El.append_children viewport
    [ layer_el; trace_host; bounds; crosshair; zoom_box; attribution ];
  El.append_children overlay [ viewport; bar ];
  El.append_children (Brr.Document.body Brr.G.document) [ overlay ];

  let levels : (int, tile_level) Hashtbl.t = Hashtbl.create 4 in
  let cur_level = ref None in
  (* Level and tile range of the last render, which is what "no gaps left" is
     judged against. *)
  let view = ref None in

  let is_open () = El.class' (Jstr.v "visible") overlay in

  (* Cached rather than read per render. Reading [clientWidth] after a render has
     written styles forces a synchronous layout, and a drag pays it every frame;
     a [ResizeObserver] costs nothing and catches every reason the viewport
     changes -- the window, a rotation, the bar reflowing, the error line
     appearing -- not just the window resizing. *)
  let vp_w = ref 0. and vp_h = ref 0. in
  let measure_viewport () =
    let jv = El.to_jv viewport in
    vp_w := Jv.to_float (Jv.get jv "clientWidth");
    vp_h := Jv.to_float (Jv.get jv "clientHeight")
  in
  let viewport_size () = (!vp_w, !vp_h) in

  (* Zoom at which the region just covers the viewport: zooming further out
     would only add area that cannot be selected. *)
  let min_zoom r (vw, vh) =
    let w0 = lon_to_px ~level:0 r.max_lon -. lon_to_px ~level:0 r.min_lon in
    let h0 = lat_to_px ~level:0 r.min_lat -. lat_to_px ~level:0 r.max_lat in
    if vw <= 0. || vh <= 0. || w0 <= 0. || h0 <= 0. then 0.
    else
      clampf 0. (float max_level) (Float.log2 (Float.max (vw /. w0) (vh /. h0)))
  in

  let clamp_view () =
    let r = !region in
    zoom := clampf (min_zoom r (viewport_size ())) (float max_level) !zoom;
    center_lat := clampf r.min_lat r.max_lat !center_lat;
    center_lon := clampf r.min_lon r.max_lon !center_lon
  in

  (* The tile level is the nearest integer to the fractional zoom, the remainder
     being taken up by a scale on the layers. Kept as functions so the drag
     handler converts screen pixels with exactly what the last render used. *)
  let current_level () =
    clampi 0 max_level (int_of_float (Float.round !zoom))
  in
  let current_scale () = Float.pow 2. (!zoom -. float (current_level ())) in

  let clear_error () = El.set_class (Jstr.v "visible") false error_msg in

  (* Tracks as read when the map was opened, and their projection for one zoom
     level: (level, anchor x, anchor y, path data). Projecting once per level and
     leaving panning to the transform is what keeps a long trace affordable --
     reprojecting some thousands of points every frame of a glide would cost more
     than the tiles do. The anchor is the first point, so the coordinates stay
     near the trace rather than out at the Mercator origin. *)
  let traces_now = ref [] in
  let trace_path = ref None in

  let project_traces level =
    match List.find_opt (fun pts -> Array.length pts > 0) !traces_now with
    | None -> None
    | Some first ->
        let lat0, lon0 = first.(0) in
        let ax = Float.round (lon_to_px ~level lon0) in
        let ay = Float.round (lat_to_px ~level lat0) in
        let b = Buffer.create 4096 in
        List.iter
          (fun pts ->
            let n = Array.length pts in
            if n >= 2 then begin
              let lx = ref 0. and ly = ref 0. in
              Array.iteri
                (fun i (lat, lon) ->
                  let x = lon_to_px ~level lon -. ax in
                  let y = lat_to_px ~level lat -. ay in
                  (* Points landing within a pixel of the last one kept are
                     dropped: zoomed out, a dense track is thousands of them
                     inside a few hundred pixels. The ends are always kept, so
                     the line still starts and finishes where the track does. *)
                  if
                    i = 0
                    || i = n - 1
                    || Float.abs (x -. !lx) +. Float.abs (y -. !ly) >= 1.
                  then begin
                    Buffer.add_string b
                      (Printf.sprintf "%c%.1f %.1f "
                         (if i = 0 then 'M' else 'L')
                         x y);
                    lx := x;
                    ly := y
                  end)
                pts
            end)
          !traces_now;
        if Buffer.length b = 0 then None
        else Some (level, ax, ay, Buffer.contents b)
  in

  let refresh_traces level =
    let stale =
      match !trace_path with Some (l, _, _, _) -> l <> level | None -> true
    in
    if stale then begin
      trace_path := project_traces level;
      let d = match !trace_path with Some (_, _, _, d) -> d | None -> "" in
      List.iter (fun shape -> set_attr shape "d" d) trace_shapes
    end
  in

  let loaded_class = Jstr.v "loaded" in

  (* Every tile of the live level's visible range is in and revealed, so nothing
     underneath can still be showing through. *)
  let level_complete () =
    match !view with
    | None -> false
    | Some (level, x0, x1, y0, y1) -> (
        match Hashtbl.find_opt levels level with
        | None -> false
        | Some lv ->
            let complete = ref true in
            for x = x0 to x1 do
              for y = y0 to y1 do
                match Hashtbl.find_opt lv.tiles (x, y) with
                | Some el when El.class' loaded_class el -> ()
                | _ -> complete := false
              done
            done;
            !complete)
  in

  let drop_levels_except keep =
    (* Collected first: removing while iterating the table is not safe. *)
    let doomed =
      Hashtbl.fold
        (fun l lv acc -> if List.mem l keep then acc else (l, lv) :: acc)
        levels []
    in
    List.iter
      (fun (l, lv) ->
        El.remove lv.container;
        Hashtbl.remove levels l)
      doomed
  in

  let prune_levels () =
    match !view with
    | Some (level, _, _, _, _) when level_complete () ->
        drop_levels_except [ level ]
    | _ -> ()
  in

  let update_layer lv ~level ~x0 ~x1 ~y0 ~y1 ~ox ~oy =
    let wanted = Hashtbl.create 64 in
    (* Tile positions are relative to the level's origin, which only moves when
       the visible range crosses a tile boundary -- a handful of frames out of a
       drag. Rewriting them regardless cost two style writes per image per frame,
       the largest slice of a drag's script time. *)
    let moved_origin = lv.ox <> ox || lv.oy <> oy in
    let place el x y =
      El.set_inline_style (Jstr.v "left") (px (float (x * tile_px) -. ox)) el;
      El.set_inline_style (Jstr.v "top") (px (float (y * tile_px) -. oy)) el
    in
    for x = x0 to x1 do
      for y = y0 to y1 do
        let key = (x, y) in
        Hashtbl.replace wanted key ();
        match Hashtbl.find_opt lv.tiles key with
        | Some el -> if moved_origin then place el x y
        | None ->
            let el = El.div ~at:At.[ class' (Jstr.v "map-tile") ] [] in
            (* The two images are revealed together: relief shaded straight
                 onto the empty background, its basemap tile not yet in, reads as
                 a blank grey block. Both are always requested, so the count is
                 fixed rather than incremented as they are built -- an image load
                 cannot complete before this returns, but it costs nothing to not
                 depend on that. *)
            let pending = ref 2 in
            let settle () =
              decr pending;
              if !pending <= 0 then begin
                El.set_class loaded_class true el;
                (* This tile may have been the last gap the level below was
                     still showing through. *)
                prune_levels ()
              end
            in
            let image l cls =
              let img =
                El.img
                  ~at:
                    At.
                      [
                        class' (Jstr.v cls);
                        src (Jstr.v (tile_url l ~level ~row:y ~col:x));
                      ]
                  ()
              in
              El.set_at (Jstr.v "draggable") (Some (Jstr.v "false")) img;
              El.set_at (Jstr.v "decoding") (Some (Jstr.v "async")) img;
              El.set_at (Jstr.v "alt") (Some Jstr.empty) img;
              ignore (Ev.listen Ev.load (fun _ -> settle ()) (El.as_target img));
              (* A tile outside coverage answers 404 and requests fail
                   transiently; either way hide the image, so the browser's
                   broken-image glyph never shows, and let the tile through --
                   the relief is absent over much of the country, LiDAR HD being
                   a survey still under way. *)
              ignore
                (Ev.listen img_error_ev
                   (fun _ ->
                     El.set_inline_style (Jstr.v "visibility") (Jstr.v "hidden")
                       img;
                     settle ())
                   (El.as_target img));
              img
            in
            El.append_children el
              [
                image base_layer "map-tile-base";
                image relief_layer "map-tile-relief";
              ];
            El.append_children lv.container [ el ];
            Hashtbl.replace lv.tiles key el;
            place el x y
      done
    done;
    lv.ox <- ox;
    lv.oy <- oy;
    (* Only this level is swept. A retained level keeps whatever it has until it
       is dropped whole, which bounds it to the screenful it was laid out with. *)
    Hashtbl.filter_map_inplace
      (fun key el ->
        if Hashtbl.mem wanted key then Some el
        else begin
          El.remove el;
          None
        end)
      lv.tiles
  in

  let render () =
    clamp_view ();
    let r = !region in
    let vw, vh = viewport_size () in
    let level = current_level () in
    let scale = current_scale () in
    let cx = lon_to_px ~level !center_lon in
    let cy = lat_to_px ~level !center_lat in
    let hw = vw /. (2. *. scale) and hh = vh /. (2. *. scale) in
    let last = (1 lsl level) - 1 in
    let tile_of v = int_of_float (Float.floor (v /. float tile_px)) in
    let x0 = clampi 0 last (tile_of (cx -. hw) - 1) in
    let x1 = clampi 0 last (tile_of (cx +. hw) + 1) in
    let y0 = clampi 0 last (tile_of (cy -. hh) - 1) in
    let y1 = clampi 0 last (tile_of (cy +. hh) + 1) in
    (* Tiles are placed relative to the first visible one rather than to the
       Mercator origin: at level 18 the latter is some 34 million pixels away,
       past the range browsers lay out reliably. The difference is folded into
       the layer transform, where it is a float. *)
    let ox = float (x0 * tile_px) and oy = float (y0 * tile_px) in
    let lv =
      match Hashtbl.find_opt levels level with
      | Some lv -> lv
      | None ->
          let container = El.div ~at:At.[ class' (Jstr.v "map-level") ] [] in
          let lv = { container; tiles = Hashtbl.create 64; ox; oy } in
          El.append_children layer_el [ container ];
          Hashtbl.replace levels level lv;
          lv
    in
    if !cur_level <> Some level then begin
      (* Exactly one level is kept as a backdrop, the one just left: holding on
         to more would pile up containers when zooming through several levels. *)
      drop_levels_except
        (match !cur_level with
        | Some prev -> [ level; prev ]
        | None -> [ level ]);
      (* Last child paints on top, so the level coming in covers the backdrop as
         its tiles arrive. Re-appending an existing child moves it. *)
      El.append_children layer_el [ lv.container ];
      cur_level := Some level
    end;
    view := Some (level, x0, x1, y0, y1);
    update_layer lv ~level ~x0 ~x1 ~y0 ~y1 ~ox ~oy;

    (* Every level is re-transformed, the retained one included, so the backdrop
       stays registered with the map through panning and fractional zooming. Each
       uses its own pixel scale and the origin its tiles were laid out with. *)
    Hashtbl.iter
      (fun l other ->
        let s = Float.pow 2. (!zoom -. float l) in
        let lcx = lon_to_px ~level:l !center_lon in
        let lcy = lat_to_px ~level:l !center_lat in
        El.set_inline_style (Jstr.v "transform")
          (Jstr.v
             (Printf.sprintf
                "translate(%.2fpx, %.2fpx) scale(%.5f) translate(%.2fpx, \
                 %.2fpx)"
                (vw /. 2.) (vh /. 2.) s (other.ox -. lcx) (other.oy -. lcy)))
          other.container)
      levels;
    prune_levels ();

    (* Same transform as a tile level, about the trace's own anchor. Unitless:
       this is SVG's transform attribute, which rejects px. *)
    refresh_traces level;
    (match !trace_path with
    | Some (_, ax, ay, _) ->
        set_attr trace_group "transform"
          (Printf.sprintf
             "translate(%.2f %.2f) scale(%.5f) translate(%.2f %.2f)" (vw /. 2.)
             (vh /. 2.) scale (ax -. cx) (ay -. cy))
    | None -> ());

    (* The bounds rectangle lives in untransformed viewport pixels so its outline
       keeps its width at every zoom.

       Clamped to just beyond the viewport rather than to some large number: the
       dimming is a very wide shadow around this element, so wherever the element
       is, moving it invalidates the whole screen -- which on a drag meant a full
       repaint every frame, the largest single cost there was. Held just outside,
       an edge that is off screen stays off screen, and once the region covers the
       viewport the element covers it too and no dimming shows, which is the
       correct picture and costs nothing to paint. The margin only has to exceed
       the border width. *)
    let screen_x lon = (vw /. 2.) +. ((lon_to_px ~level lon -. cx) *. scale) in
    let screen_y lat = (vh /. 2.) +. ((lat_to_px ~level lat -. cy) *. scale) in
    let margin = 8. in
    let bl = clampf (-.margin) (vw +. margin) (screen_x r.min_lon) in
    let br = clampf (-.margin) (vw +. margin) (screen_x r.max_lon) in
    let bt = clampf (-.margin) (vh +. margin) (screen_y r.max_lat) in
    let bb = clampf (-.margin) (vh +. margin) (screen_y r.min_lat) in
    El.set_inline_style (Jstr.v "left") (px bl) bounds;
    El.set_inline_style (Jstr.v "top") (px bt) bounds;
    El.set_inline_style (Jstr.v "width") (px (Float.max 0. (br -. bl))) bounds;
    El.set_inline_style (Jstr.v "height") (px (Float.max 0. (bb -. bt))) bounds;

    (* [set_children] would build and swap a text node every frame. *)
    Jv.set (El.to_jv coord) "textContent"
      (Jv.of_string (Printf.sprintf "%.5f, %.5f" !center_lat !center_lon));

    (* After [clamp_view], so what is remembered is a view that can be returned
       to rather than one the region would immediately pull back. *)
    saved_of !region := (!center_lat, !center_lon, !zoom)
  in

  (* Moves the map by a screen-pixel delta, the way dragging it does: divided by
     the current scale, since that is what stands between screen pixels and the
     tile level's own. *)
  let pan_by ~dx ~dy =
    let level = current_level () in
    let scale = current_scale () in
    let cx = lon_to_px ~level !center_lon -. (dx /. scale) in
    let cy = lat_to_px ~level !center_lat -. (dy /. scale) in
    center_lon := px_to_lon ~level cx;
    center_lat := px_to_lat ~level cy
  in

  let rec glide_step mine prev_t =
    if mine = !glide then
      let t = now_ms () in
      let dt = t -. prev_t in
      let again t =
        ignore (Brr.G.request_animation_frame (fun _ -> glide_step mine t))
      in
      if dt <= 0. then again prev_t
      else begin
        (* Integrate the decay over the frame rather than stepping by v*dt: the
           exact displacement of v*exp(-t/tau) is v*tau*(1-exp(-dt/tau)), which
           makes the whole glide cover v0*tau however long the frames turn out to
           be. Stepping by v*dt instead loses distance as frames lengthen, and a
           fast fling lengthens them by needing more tiles -- which had the
           absurd result of a brisk flick gliding less far than a gentle one. *)
        let decay = exp (-.dt /. glide_tau) in
        let travel = glide_tau *. (1. -. decay) in
        let was_lat = !center_lat and was_lon = !center_lon in
        pan_by ~dx:(!vel_x *. travel) ~dy:(!vel_y *. travel);
        vel_x := !vel_x *. decay;
        vel_y := !vel_y *. decay;
        render ();
        (* Stopping on "did not move" is what handles the region edge: the clamp
           in [render] pins the centre there and the fling would otherwise keep
           running invisibly to its threshold. *)
        let moved = !center_lat <> was_lat || !center_lon <> was_lon in
        if moved && (Float.abs !vel_x > 0.01 || Float.abs !vel_y > 0.01) then
          again t
        else stop_glide ()
      end
  in

  let start_glide () =
    (* A pointer held still before release must not fling. No move events arrive
       while it is stationary, so the last velocity measured would be a stale
       reading of a gesture that had already stopped. *)
    if
      now_ms () -. !last_move_ms < 100.
      && (Float.abs !vel_x > 0.02 || Float.abs !vel_y > 0.02)
    then begin
      incr glide;
      glide_step !glide (now_ms ())
    end
    else stop_glide ()
  in

  (* Zoom always keeps the centre fixed, whatever drives it: the crosshair is
     the target being aimed, so anchoring anywhere else would push it off. *)
  let zoom_by delta =
    stop_glide ();
    zoom := !zoom +. delta;
    render ()
  in

  let region_buttons =
    List.map
      (fun r ->
        let btn =
          El.button
            ~at:At.[ class' (Jstr.v "map-region-btn") ]
            [ El.txt (Jstr.v r.name) ]
        in
        (r, btn))
      regions
  in
  let sync_region_buttons () =
    List.iter
      (fun (r, btn) -> El.set_class (Jstr.v "active") (r == !region) btn)
      region_buttons
  in
  El.set_children region_row (List.map snd region_buttons);

  let contains r lat lon =
    lat >= r.min_lat && lat <= r.max_lat && lon >= r.min_lon && lon <= r.max_lon
  in

  let goto r =
    let switched = !region != r in
    region := r;
    (* A fling in progress belongs to the view being left. *)
    stop_glide ();
    (* Keeping a backdrop across a region switch would leave the Alps showing
       under Reunion: the point of retaining a level is that it shows the same
       ground as the one replacing it. *)
    if switched then begin
      drop_levels_except [];
      cur_level := None;
      view := None
    end;
    sync_region_buttons ();
    clear_error ();
    render ()
  in

  (* Switching regions returns to wherever that region was last left, scale
     included. Picking the region already shown restores what is on screen, the
     view having been recorded by the last render. *)
  let enter_saved r =
    let lat, lon, z = !(saved_of r) in
    center_lat := lat;
    center_lon := lon;
    zoom := z;
    goto r
  in

  (* Opening the map on a position: that position if the region holds it, else
     wherever the region was last left -- a rectangle bounding an irregular coast
     has much of its area at sea, so clamping onto its boundary, or taking its
     middle, can land in open water. The scale is the region's remembered one
     either way, so reopening resumes at the scale last used there. *)
  let enter_at r ~lat ~lon =
    let slat, slon, z = !(saved_of r) in
    zoom := z;
    if contains r lat lon then begin
      center_lat := lat;
      center_lon := lon
    end
    else begin
      center_lat := slat;
      center_lon := slon
    end;
    goto r
  in

  List.iter
    (fun (r, btn) ->
      ignore
        (Ev.listen Ev.click
           (fun ev ->
             Ev.stop_propagation ev;
             enter_saved r)
           (El.as_target btn)))
    region_buttons;

  (* Pointer handling: down on the viewport, move and up on the document, so a
     drag released outside the window cannot leave a pointer stuck down. Only
     pointers that started on the viewport are ever tracked. *)
  let pointers : (int, float * float) Hashtbl.t = Hashtbl.create 4 in
  let pinch = ref None in

  let pointer_span () =
    match Hashtbl.fold (fun _ pos acc -> pos :: acc) pointers [] with
    | (x1, y1) :: (x2, y2) :: _ -> Some (Float.hypot (x2 -. x1) (y2 -. y1))
    | _ -> None
  in

  ignore
    (Ev.listen Ev.pointerdown
       (fun ev ->
         let p = Ev.as_type ev in
         let m = Ev.Pointer.as_mouse p in
         Ev.prevent_default ev;
         Ev.stop_propagation ev;
         (* Touching the map catches it, as grabbing a spinning thing does. This
            also clears the velocity, so a pinch -- whose moves never measure one
            -- cannot be released into a fling left over from an earlier drag. *)
         stop_glide ();
         last_move_ms := now_ms ();
         Hashtbl.replace pointers (Ev.Pointer.id p)
           (Ev.Mouse.client_x m, Ev.Mouse.client_y m);
         pinch :=
           if Hashtbl.length pointers >= 2 then
             match pointer_span () with
             | Some d when d > 0. -> Some (d, !zoom)
             | _ -> None
           else None)
       (El.as_target viewport));

  ignore
    (Ev.listen Ev.pointermove
       (fun ev ->
         let p = Ev.as_type ev in
         let id = Ev.Pointer.id p in
         match Hashtbl.find_opt pointers id with
         | None -> ()
         | Some (prev_x, prev_y) ->
             Ev.stop_propagation ev;
             let m = Ev.Pointer.as_mouse p in
             let x = Ev.Mouse.client_x m and y = Ev.Mouse.client_y m in
             Hashtbl.replace pointers id (x, y);
             if Hashtbl.length pointers >= 2 then
               begin match (!pinch, pointer_span ()) with
               | Some (d0, z0), Some d when d > 0. ->
                   zoom := z0 +. Float.log2 (d /. d0);
                   render ()
               | _ -> ()
               end
             else begin
               let dx = x -. prev_x and dy = y -. prev_y in
               let t = now_ms () in
               let dt = t -. !last_move_ms in
               last_move_ms := t;
               (* Smoothed over a 50 ms window rather than taken from the last
                  pair of samples, so an uneven pointer rate does not turn into
                  an erratic fling. *)
               if dt > 0. then begin
                 let a = 1. -. exp (-.dt /. 50.) in
                 vel_x := (dx /. dt *. a) +. (!vel_x *. (1. -. a));
                 vel_y := (dy /. dt *. a) +. (!vel_y *. (1. -. a))
               end;
               pan_by ~dx ~dy;
               render ()
             end)
       (Brr.Document.as_target Brr.G.document));

  let release ev =
    let p = Ev.as_type ev in
    let id = Ev.Pointer.id p in
    if Hashtbl.mem pointers id then begin
      Hashtbl.remove pointers id;
      (* Dropping to one finger restarts the drag from where it is rather than
         carrying the pinch baseline over. *)
      if Hashtbl.length pointers < 2 then pinch := None;
      (* Only once nothing is touching the map: letting go of one finger of a
         pinch leaves the other still dragging. *)
      if Hashtbl.length pointers = 0 then start_glide ()
    end
  in
  ignore
    (Ev.listen Ev.pointerup release (Brr.Document.as_target Brr.G.document));
  ignore
    (Ev.listen Ev.pointercancel release (Brr.Document.as_target Brr.G.document));

  ignore
    (Ev.listen Ev.wheel
       (fun ev ->
         Ev.prevent_default ev;
         Ev.stop_propagation ev;
         let w = Ev.as_type ev in
         let mode = Ev.Wheel.delta_mode w in
         let dy = Ev.Wheel.delta_y w in
         (* The three delta modes are wildly different units; normalise so one
            mouse notch is a fraction of a level and a trackpad stays smooth. *)
         let step =
           if mode = Ev.Wheel.Delta_mode.pixel then dy /. 300.
           else if mode = Ev.Wheel.Delta_mode.line then dy /. 3.
           else dy
         in
         zoom_by (-.step))
       (El.as_target viewport));

  ignore
    (Ev.listen Ev.dblclick
       (fun ev ->
         Ev.prevent_default ev;
         Ev.stop_propagation ev;
         zoom_by 1.)
       (El.as_target viewport));

  ignore
    (Ev.listen Ev.click
       (fun ev -> Ev.stop_propagation ev)
       (El.as_target zoom_in));
  ignore
    (Ev.listen Ev.click
       (fun ev -> Ev.stop_propagation ev)
       (El.as_target zoom_out));
  ignore (Ev.listen Ev.click (fun _ -> zoom_by 1.) (El.as_target zoom_in));
  ignore (Ev.listen Ev.click (fun _ -> zoom_by (-1.)) (El.as_target zoom_out));

  let close () =
    El.set_class (Jstr.v "visible") false overlay;
    Hashtbl.reset pointers;
    pinch := None;
    (* Or the map would still be drifting behind a hidden overlay, and would be
       found mid-glide on reopening. *)
    stop_glide ();
    (* Without this the focus would stay on a hidden overlay and the menu behind
       would stop answering its own arrow keys. *)
    Option.iter (fun el -> ignore (Jv.call el "focus" [||])) !restore_focus;
    restore_focus := None
  in

  let confirm () =
    let lat = !center_lat and lon = !center_lon in
    if in_range ~lat ~lon then begin
      close ();
      on_select ~lat ~lon
    end
    else begin
      (* The regions are meant to be inside the renderer's coverage, so this is
         the two disagreeing rather than anything the user can fix. *)
      El.set_children error_msg
        [ El.txt (Jstr.v "No elevation data at this position") ];
      El.set_class (Jstr.v "visible") true error_msg
    end
  in

  ignore (Ev.listen Ev.click (fun _ -> close ()) (El.as_target btn_cancel));
  ignore (Ev.listen Ev.click (fun _ -> confirm ()) (El.as_target btn_ok));

  (* Keys are taken on the document in the capture phase, so that while the map
     is open it owns the keyboard outright. Bubbling would not do: the menu the
     map is opened from stays behind it with its coordinate input focused, and
     that input's handlers stop propagation on the arrows, Enter and Escape --
     precisely the keys wanted here. Capture also puts this ahead of the camera
     controls bound to the window, which use the same arrows and +/-. Handled
     keys stop there, so nothing underneath sees them. *)
  ignore
    (Ev.listen
       ~opts:(Ev.listen_opts ~capture:true ())
       Ev.keydown
       (fun ev ->
         if is_open () then begin
           let step () =
             (* A tenth of the viewport per press, independent of zoom. *)
             let _, vh = viewport_size () in
             vh /. 10. /. current_scale ()
           in
           (* In tile-level pixels, [step] having already divided by the scale;
              unlike [pan_by], which takes a pointer's screen delta. *)
           let pan ~dx ~dy =
             stop_glide ();
             let level = current_level () in
             let cx = lon_to_px ~level !center_lon +. dx in
             let cy = lat_to_px ~level !center_lat +. dy in
             center_lon := px_to_lon ~level cx;
             center_lat := px_to_lat ~level cy;
             render ()
           in
           let key = Jstr.to_string (Ev.Keyboard.code (Ev.as_type ev)) in
           let handled = ref true in
           (match key with
           | "ArrowLeft" -> pan ~dx:(-.step ()) ~dy:0.
           | "ArrowRight" -> pan ~dx:(step ()) ~dy:0.
           | "ArrowUp" -> pan ~dx:0. ~dy:(-.step ())
           | "ArrowDown" -> pan ~dx:0. ~dy:(step ())
           | "Equal" | "NumpadAdd" -> zoom_by 1.
           | "Minus" | "NumpadSubtract" -> zoom_by (-1.)
           | "Enter" | "NumpadEnter" -> confirm ()
           | "Escape" -> close ()
           | _ -> handled := false);
           if !handled then begin
             Ev.prevent_default ev;
             Ev.stop_propagation ev
           end
         end)
       (Brr.Document.as_target Brr.G.document));

  (* Observing the element rather than listening for window resizes: the viewport
     also changes when the bar reflows or the error line appears, neither of which
     resizes the window, and this is what keeps [vp_w]/[vp_h] true without a read
     per render. *)
  let observer_cb =
    (* [Jv.callback], not [Jv.repr]: the latter hands the OCaml closure over as a
       value, which is not a callable JS function. *)
    Jv.callback ~arity:1 (fun _entries ->
        measure_viewport ();
        if is_open () then render ())
  in
  let observer =
    Jv.new' (Jv.get Jv.global "ResizeObserver") [| observer_cb |]
  in
  ignore (Jv.call observer "observe" [| El.to_jv viewport |]);

  fun ~lat ~lon ->
    let r =
      match List.find_opt (fun r -> contains r lat lon) regions with
      | Some r -> r
      | None ->
          (* Nearest by view point: an equirectangular comparison is ample, the
             regions being thousands of kilometres apart. *)
          let score r =
            let dlat = lat -. r.view_lat in
            let dlon = (lon -. r.view_lon) *. cos (lat *. pi /. 180.) in
            (dlat *. dlat) +. (dlon *. dlon)
          in
          List.fold_left
            (fun best r -> if score r < score best then r else best)
            first_region regions
    in
    (* Read afresh: traces are added and toggled from a panel the map covers, so
       between two openings the set can be anything. *)
    traces_now := traces ();
    trace_path := None;
    (restore_focus :=
       let active = Jv.get (Jv.get Jv.global "document") "activeElement" in
       if Jv.is_none active then None else Some active);
    El.set_class (Jstr.v "visible") true overlay;
    El.set_has_focus true overlay;
    (* Before the first render, in case the observer has not reported yet. *)
    measure_viewport ();
    (* No zoom reset: [enter_at] takes the region's remembered scale, so the map
       reopens where the last look at that region left off. *)
    enter_at r ~lat ~lon
