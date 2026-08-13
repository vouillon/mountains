# Web viewer fix plan (from src/web review, 2026-07-29)

Findings from a full review of `src/web/` (renderer, loaders/workers, shaders,
service worker). Ordered as an implementation plan: each phase is independently
shippable, highest impact first. Line numbers refer to HEAD as of commit
e22f974.

## Phase 1 — Async error handling (silent hangs) — DONE (uncommitted)

Implemented 2026-07-29. Notes: `to_lwt` had five copies, not three (also
`worker.ml:92` and a local one in `dem_loader.ml`'s `prefetch`); kept as local
fixes, no shared module (worker is a separate executable stanza). DEM coverage
bounds in `viewer.ml:2231` were corrected to the actual dataset extent
(lon [5,10) and [54,57)) since the strict extent check would otherwise have
made Réunion unreachable. Failed sub-tile regions read as −500 m (u16 zero);
pre-fill with 3449 if sea level is preferred.

- [x] **Fix `to_lwt` to fail the Lwt task instead of raising.**
      Three copies: `worker.ml:44`, `clc_loader.ml:8`, `reader.ml:5`.
      Each does `raise (Jv.Error err)` inside the `Fut.await` callback, which
      becomes an unhandled JS rejection and leaves the Lwt task pending
      forever. Replace with `Lwt.wakeup_exn u (Jv.Error err)`.
      Consequences today: failed fetch → app stuck on "Loading Terrain…";
      failed WASM init / corrupt gzip in a worker → pool slot leaked forever
      (after 4, all tile loading hangs); rejected `Cache.add` → prefetch chain
      stalls. Consider deduplicating the helper afterwards.

- [x] **Check HTTP status on tile fetches.**
      `dem_loader.ml:118-128` and `clc_loader.ml:259` never check
      `Response.ok`; a 404 ships an HTML error page to the decoder as tile
      data.

- [x] **Tolerate individual missing DEM sub-tiles.**
      One failed sub-tile fails `Lwt.join !tasks` (`dem_loader.ml:187`) and
      aborts the entire heightmap; `viewer.ml:3168` has no catch → eternal
      loading screen. Missing edge tiles should degrade (e.g. flat/zero
      heights) rather than abort.

- [x] **`Dem_loader.in_range` must check the full tile extent, not the center
      point.** `dem_loader.ml:280` ignores `size`; near a coverage edge,
      `load` requests tiles outside the dataset → 404 → total failure via the
      bug above.

## Phase 2 — Service worker / PWA

Implemented 2026-07-29. Notes: two caches instead of one — the app shell lives
in `mountains-<digest>` (renewed on every deploy, older ones dropped in
`activate`), the immutable tiles stay in `"v1"`, whose name is also hard-coded
in the three main-thread prefetch sites (`dem_loader.ml`, `clc_loader.ml`,
`reader.ml`); `activate` also prunes the non-tile entries the previous worker
had left in `"v1"`. The live DEM tiles are `.dem` (`.tif` only covers
`reader.ml`'s dormant Copernicus path), so cache-first now matches
`.dem`/`.clc`/`.tif`. `FetchEvent.preloadResponse` also had to be guarded: it
is Chromium-only and `Fut.of_promise` raised on it, aborting the whole fetch
handler in Firefox.

Not runtime-tested (no browser in the dev environment): activation ordering,
the Firefox paths, quota-exceeded behavior — do one manual devtools pass
(returning client picks up a new worker; app starts over plain http) before
relying on it. Two pre-existing quirks were left alone; see Phase 5.

- [x] **Version the cache and refresh cache-first assets on deploy.**
      `service_worker.ml:4` uses an immortal `"v1"` cache; `worker.bc.js` and
      `decompress_tile.wasm` are cache-first (`:74-78`) and only refreshed by
      the install handler, which runs only when `service_worker.bc.js` itself
      changes bytes. A deploy changing the worker but not the SW strands
      returning users on a stale worker (viewer↔worker protocol breakage,
      unrecoverable without clearing site data). Plan: embed a build hash in
      the SW source (bumps every deploy), use it as the cache name, delete old
      caches in `activate`.

- [x] **Make `cache.put` failures non-fatal.**
      `service_worker.ml:35,44,51`: the network response is returned only if
      `put_in_cache` succeeds, so `QuotaExceededError` turns successful
      downloads into `Response.error ()` forever after. Return the response
      regardless; treat the put as best-effort.

- [x] **Precache the hashed wasm module.**
      Install list (`service_worker.ml:110-117`) omits
      `viewer.bc.wasm.assets/code-<hash>.wasm`, which also can't be cached by
      the fetch handler on the first visit (loaded before the SW claims the
      page). Offline fails after a single visit. Needs the hashed filename
      injected at build time (dune rule), or a manifest the SW reads.

- [x] **Don't hard-block startup on the service worker.**
      `viewer.ml:3128` awaits `Container.ready` (never resolves if
      registration fails: Firefox private browsing, SW 404, iOS Lockdown), and
      `Container.register` at top-level (`viewer.ml:3302-3307`) throws on
      insecure origins where `navigator.serviceWorker` is undefined (http LAN
      testing → black screen). Guard the registration, add a timeout or
      proceed-without-SW path.

- [x] **Add CLC *and DEM* tiles to the cache-first list.**
      `service_worker.ml:74-78` only matches `.tif` / `worker.bc.js` /
      `decompress_tile.wasm`; both `data/clc/*.clc` and the live heightmap
      tiles `data/dem/*.dem` (dem_loader.ml) go network-first, so every
      session re-downloads them. The `.tif` rule only covers reader.ml's
      raw-Copernicus path, which has no live caller in the web client.
      (Comment at `:121` still documents the old `.tif`/`.geojson` scheme —
      update it.)

- [x] **Two-cache design (constraint on the versioning item).**
      dem_loader.ml:291, clc_loader.ml:166, reader.ml:88 hard-code cache
      "v1" for prefetch. Keep "v1" as a persistent data cache for immutable
      tiles (survives deploys, preserves users' prefetched tiles); use the
      versioned cache only for app-shell assets; activate deletes caches
      that are neither.

- [x] **Firefox: guard `navigationPreload`.**
      `service_worker.ml:92-96` evaluates
      `Navigation_preload_manager.enable` on an undefined
      `registration.navigationPreload`, throwing before `Clients.claim` runs.

## Phase 3 — iOS shader precision (mediump = fp16 on device)

Implemented 2026-07-29. Notes: two extra one-line edits were required for
consistency. (a) `terrain.frag:4` also declares `relief`; uniform precision
*must* match across the stages of a program (GLSL ES 3.00 §4.3.5), so it had to
become `highp` too or the terrain program would no longer link. (b) `sky.vert:2`
declares `out mediump vec2 v_uv`; varying precision need *not* match across
stages, but the interpolated precision is the *minimum* of the two (§4.3.10), so
making only the fragment-side `v_uv` highp would have had no effect. Left alone:
`sky_params` stays mediump (constant per frame; fp16 only perturbs the effective
FOV by ~5e-4, no per-pixel discontinuity), and `mie`/`halo` stay mediump — the
`pow` is evaluated at highp because `cos_gamma` is (§8 built-in precision rules)
and only the small result is narrowed. Not runtime-tested: no iOS device here.

- [x] **`radial_common.vert:16`: `mediump sampler2D relief` → `highp`.**
      Relief texels are world-space heights; fp16 rounding gives up to
      ±2.3 m per-texel vertical jitter above ~4200 m. Violates the project
      rule "highp for world coordinates".

- [x] **`sky.frag`: compute the view ray / `cos_gamma` in highp.**
      File-level `precision mediump float` (`sky.frag:2`) puts the
      `mat3(inv_view) * view_ray` product and `u_lightDir` at fp16; the sun
      disc cutoff window (`cos_gamma > 0.9995`) is narrower than the fp16 ulp
      near 1.0 → ragged sun disc, banded halo on iOS.

- [x] **`ao_bake.frag:7-21`: IGN dither in highp.**
      `IGN(mediump vec2 p)` with `gl_FragCoord` up to ~2048 collapses the
      noise to ~16 distinct values at fp16 → directional banding in baked AO.

Follow-up found during implementation, not yet done:

- [x] `ao_bake.frag:1` and `ao_blur.frag:3` declare `uniform sampler2D relief;`
      with no precision qualifier; samplers default to **lowp** in fragment
      shaders, so both AO passes fetch heights at lowp. Possibly benign (lowp's
      8 fractional bits can represent RG8 texel values exactly, but that is
      implementation-dependent); make them `highp` for consistency with the
      Phase 3 changes.

Second pass (2026-07-31, uncommitted), prompted by banding reported on a phone
and absent on desktop. Three `terrain.frag` locals held a world or texture
coordinate at the file-default mediump; all are one-word fixes and all are
provably no-ops on desktop (RMSE exactly 0 on the pinned captures, since
desktop mediump is fp32):

- [x] `macroPos = v_world_pos.xy * 0.02` — world metres scaled, so it reaches
      ~1400 at the tile edge, where the fp16 ulp is a whole unit. The three
      `sin`/`cos` phases taken from it snap in ~1 radian steps, breaking the
      grass patch pattern into flat blocks over distant grass. Best candidate
      for the reported artefact.
- [x] `levelUV` / `dist_from_center` / `d_center` / `frac` in
      `sampleCLCBilinear` — the CLC clipmap texture coordinate, narrowed to
      fp16 immediately before being multiplied by the 1024-wide cover map. The
      bilinear weight then takes 2-4 values per texel instead of a ramp, so
      every land-cover transition is a staircase (~120 m at the outer levels).
- [x] `applyWaterEffects(..., vec2 worldPos)` — parameter at mediump receiving
      `v_world_pos.xy`; the foam UV snaps into blocks metres across.

Third pass, same session: the reported artefact turned out to be **shadow
acne**, and the whole shadow lookup in `terrain.frag` was at mediump. Three
compounding fp16 errors, each on its own enough to band a grazing slope along
its contours, all fixed by qualifying them highp (again no-ops on desktop,
RMSE exactly 0 on three pinned captures):

- [x] `pcf_shadow(..., vec2 coords, float compare, ...)` — a shadow-map UV is
      in [0, 1] over 2048 texels, so one texel is 4.88e-4, *exactly* the fp16
      ulp there. The five PCF taps collapsed onto one texel.
- [x] `offset_pos` — a world coordinate (the project rule again), quantised to
      2 m at 3 km and 16 m at 20 km before being multiplied by the shadow
      matrix.
- [x] `s_pos` / `proj_coords` / `current_depth` — the comparison depth is in
      [0, 1] over the cascade's depth span, so fp16 quantised it to 10 m
      (cascade 0), 23 m (1) and 73 m (2) of world depth, against a bias sized
      for the 2.93/11.7/36.6 m texel spans. Whole contour bands flipped
      between lit and shadowed.

Reproducing it needed the phone's framing, not the phone: the artefact is
magnification-dependent, and the rig's default 1200x800 90-degree captures hid
it. Portrait `WINDOW=864,1730` at `zoom=3` shows it on desktop.

Still unaudited for fp16: `sky.frag`'s `cos_theta`/`horizon_factor` (steps
computed at ~0.1 LSB, under the dither, so probably fine).

## Phase 4 — Renderer correctness

Implemented 2026-07-29. Notes: the floor-based anchoring is now a single shared
helper, `Web_utils.arcsec_floor`, used by `dem_loader.ml`'s `load`, `prefetch`
and `in_range` (the latter two must keep computing exactly the same bounds as
`load`), by `clc_loader.ml`'s `prefetch`, and by the two `viewer.ml` sites (CLC
raster center, POI tile indices). Coverage was re-verified with a throwaway
model of `load`/`in_range` (`ocaml` script, both supported boxes, size 4096):
since both the acceptance test and the requested tile set factorize into
independent latitude and longitude conditions, an arcsecond/8 sweep of each
axis plus a coarse 2-D sweep suffices — every accepted position only requests
deg_lat 43..46 / deg_lon 5..9 and deg_lat −22..−21 / deg_lon 54..56, all
present in `data/dem/`, 0 missing files; Réunion positions (−21.2/55.5,
−21.115/55.48, −20.9/55.5) stay accepted. Two side findings: the per-POI
arcsecond conversion had to become `Float.round`, *not* floor — POI coordinates
are pre-rounded to whole arcseconds but the division by 3600 is inexact, so
truncate *and* floor are off by one on ~3 % of them; and
`Dem_loader.prefetch ~size:7200` still requests 7 sub-tiles outside the Réunion
dataset (a 2°-tall prefetch over 2° of data), which is pre-existing, unrelated
to the rounding, and left alone.

For the allocation item the changes are deliberately narrow: a new
`Matrix.mult_into` (allocation-free product, same arithmetic as `mult`) plus two
module-level scratch matrices cut the per-POI matrix chains from five fresh
matrices per POI per loop to one, and `screen_inclination`, the scale factors,
the rotation/scale prefixes and `compute_sub_arcsec_offset` are now computed
once per frame instead of once per POI. Left as is on purpose: the per-frame
`transform`/`proj`/`Matrix.inverse` temporaries (O(1) per frame; removing them
would need in-place variants of `project`/`inverse`/`rotation_matrix`), the two
rebuilt POI lists (restructuring, not hoisting), the `Matrix.vector` records in
the projection filter, and the remaining `translate x y 0.` per POI. Matrices,
multiplication order and values are unchanged; not runtime-tested (no browser
here).

- [x] **Negative-latitude off-by-one (`floor` vs `truncate`).**
      `dem_loader.ml:58` anchors the tile center with `truncate (lat *. 3600.)`;
      `render_state.ml:27` computes sub-arcsecond offsets with `floor`. They
      differ by 1 for negative latitudes (Réunion, lat −22..−20 per
      `viewer.ml:2231`): everything lands ~31 m north of the requested
      position. Pick one convention (floor is the safer one) everywhere:
      also `dem_loader.ml:195`, `viewer.ml:1061`, `viewer.ml:3211`.

- [x] **Reject non-finite URL parameters.**
      `viewer.ml:2244-2251`: `float_of_string` accepts `"nan"`/`"1e999"`;
      `clamp_zoom` (`viewer.ml:229`) propagates NaN through IEEE min/max; NaN
      then defeats the `should_draw` change detection permanently →
      unrecoverable blank screen. Validate with `Float.is_finite`.

- [x] **Reduce per-frame allocation in `draw`.**
      `viewer.ml:1584-1714`: fresh 16-float matrices (`transform`, `proj`,
      `Matrix.inverse`, per-POI matrix chains), rebuilt POI list with
      closures, `screen_inclination` recomputed per POI per frame,
      `compute_sub_arcsec_offset` per point though constant per session.
      Pre-allocate scratch matrices; hoist per-frame constants out of the POI
      loops. (Project rule: minimize allocation in the render loop.)

## Phase 5 — Smaller cleanups (low severity)

Mechanical items implemented 2026-07-29; the four needing a design or visual
decision (faded POI markers, the shoreline `textureLod`, the `v_view_dir`
renormalization, the install-time precache size) are deliberately left
unticked. Notes: the `cache.put` clone was a false finding — Brr's
`Fetch.Response.of_response` *is* `clone()` (`brr_io.ml`: `Jv.call r "clone"`),
so `store_in_cache` has always stored a copy and returned the original to the
page; no change was needed. `w` and `grid_base` were dead only in the *web*
shaders: `src/sdl/viewer.ml` keeps its own inline copy of the radial vertex
shader, which still computes `pow(grid_base, ring)` where the web one uses
`exp(grid_k * ring)`, so `render_state.ml`/`.mli` lost
`radial_params.grid_base`, both `grid_base` and `w` uniform-location fields and
their six upload/lookup sites while `src/sdl` was left alone; the `~w` argument
of `upload_session_static` stays, it still feeds `inv_w` and `max_lod`. The
worker-pool `post` reorder is safe because both call sites wrap it in
`Lwt.finalize`, which turns a synchronous raise into a failed promise and still
releases the worker. `src/lib/loader.ml`'s `in_range` indeed has no caller
(`src/sdl` only uses `Loader.f`) and its `max_lat'`/`max_lon'` were derived from
the *degree* bounds instead of `min_lat'`/`min_lon'`; the native tile layout is
the same asymmetric one as the web loader (rows cover (deg, deg + 1] in
arcseconds, columns [deg, deg + 1)), so the fix is a shared `extent` helper —
now used by `in_range`, `prefetch` and `f` alike, since `in_range`'s whole job
is to predict `f`'s requests — carrying the floor-based anchoring of Phase 4
(`f`/`prefetch` used `truncate`; identical for the northern-hemisphere
Copernicus tiles the native path actually reads). Shader compile/link failures
log through `Brr.Console.error` and execution continues; signatures unchanged.
Not runtime-tested (no browser or iOS device here): the fullscreen fallback
path, and the CLC log that now distinguishes a 404 from a decode failure.

- [x] `service_worker.ml` (`store_in_cache` call sites) — the response handed
      to `cache.put` is not `clone()`d before being returned to the page.
      Works today (pre-existing behavior), but is against the Cache API's
      documented usage and could break on stricter implementations; clone
      before putting.
- [ ] `service_worker.ml` install handler — `add_all` downloads the whole app
      shell (~5 MB incl. Wasm) before the worker activates; on first visit
      this competes with the app's own tile loading for bandwidth (the 10 s
      startup backstop from Phase 2 only stops it from blocking startup).
      Consider precaching lazily or only the assets not fetched by the page
      itself.

- [x] `viewer.ml:1298-1321` — `rasterize_clc_tiles` never deletes its FBO and
      seven 1024×1024 depth renderbuffers (~14 MB GPU held all session;
      `compute_ao` shows the correct cleanup pattern).
- [ ] `viewer.ml:1616-1697` — overlap-suppressed POI branch is unreachable:
      the filter drops `shown = false` entries, so the faded-marker path
      (`angle = 0.`, 0.4-alpha color) never runs. Decide intent: either keep
      hidden POIs as faded markers (fix the filter) or delete the dead branch.
- [x] `viewer.ml:2640-2658` — `switch_to_fullscreen` throws uncaught on
      iPhone Safari (no `requestFullscreen`) on every tap in Sensor mode;
      wrap and ignore failure.
- [x] `web_utils.ml:73-97` — check `COMPILE_STATUS`/`LINK_STATUS` and log the
      info log; today a shader error is a silent black screen.
- [x] `clc_loader.ml:384` — `load_tiles` maps every exception to `None`,
      conflating "tile not published" with decode/worker failures; at least
      log unexpected errors.
- [x] `worker_pool.ml:35-53,89-102` — latent reply-desync traps: unknown
      message types drop the pending resolver; a synchronous `Worker.post`
      failure leaves an orphan resolver at the head of `pending`, shifting
      all later replies by one. Enqueue after a successful post; fail the
      resolver on unknown messages.
- [x] `terrain.frag:255-257,453-500` — EVALUATED, accepted risk (2026-07-30):
      quad-coherent continuous gates, worst case one wrongly-mipped decorative
      jitter sample on shoreline-edge quads; textureGrad fix documented in a
      comment above getWaterMask but not applied (slower sampling path on
      mobile). Revisit only if shoreline sparkle is observed on a device.
      Original finding: shoreline `texture()` calls inside
      branches on material-derived (texel-frequency) values; undefined
      derivatives per spec. Use `textureLod`/`textureGrad` or accept as-is
      (works on common hardware).
- [x] `terrain.frag:535-562` — renormalize `v_view_dir` in the fragment
      shader before Fresnel/specular use.
- [x] Dead code: uniforms `w` and `grid_base` (`radial_common.vert:3,10`,
      uploaded from `render_state.ml`), varying `v_h`
      (`terrain_main.vert:3,17`).
- [x] `radial_common.vert:52` — comment says "row 0 is north"; row 0 is
      south (`dem_loader.ml:153-159`). Fix the comment before it misleads a
      future lighting fix.
- [x] `src/lib/loader.ml:27-35` — `in_range` mixes degrees and arcseconds
      (`max_lat' = min_lat + size - 1`); dormant (no live caller found) but
      contradicts `loader.mli`'s contract. Fix or remove.

## Phase 6 — Candidate improvements (from 2026-07-29 analysis, not yet decided)

Low-risk starting bundle implemented 2026-07-29 (the four boxes ticked below).
Notes: only `ao_final_tex` gets the mip chain — `ao_bake_tex` is read once,
texel-for-texel, by the blur pass and is deleted immediately after, so
`create_r8_target` took an optional `~mipmap` flag instead of changing both.
`generate_mipmap` has to run after the FBO is unbound (rendering through the
FBO writes level 0 only). Nothing binds a sampler object to terrain texture unit
3, so the new `LINEAR_MIPMAP_LINEAR` filter is actually used. The PCF skip
required moving the whole shadow block from before the material system to just
after `final_l`; `normal` and `cosTheta` are never reassigned in between, so the
inputs are unchanged and the block's locals now live in the `if` scope. The two
dithers needed an explicit `highp` IGN helper in each file: both `terrain.frag`
and `sky.frag` default to `mediump`, and fp16 `gl_FragCoord` products collapse
the hash (same bug as the ao_bake fix in 4399014). `sky.frag`'s old
`fract(sin(dot(...)))` hash was replaced by IGN for consistency.

Azimuth culling implemented 2026-07-30. Notes: the drawn range is derived from
the projection every frame in `draw`, next to `snapped_alpha`. `Matrix.project`
puts `x_scale`/`y_scale` straight on `x`/`y` with `w_clip = -z`, so the frustum
half-angles are `atan (1/x_scale)` and `atan (1/y_scale)` and the whole frustum
sits inside the cone of half-angle β about the view axis with
`tan β = hypot (1/x_scale, 1/y_scale)` — the corner angle, independent of roll.
A cone of half-angle β whose axis is at elevation ε covers azimuths within
`Δ = asin (sin β / cos ε)` of the axis azimuth when `sin β < cos ε`, else it
contains the zenith and the full wedge is needed (`not (sin β < cos ε)`, so a
NaN orientation also falls back). `cos ε` is just the horizontal length of the
normalized forward vector, so no `asin`/`acos` is needed on the pitch: the
`compute_azimuth` call was inlined to reuse its `fwd` (same single record
allocation as before, nothing added). Verified numerically over 9 zooms × 13
aspects × 186 pitches (±89.5°, plus ±89.9°) × 26 rolls (full 360°) × 2 azimuths
= 1.13M frusta, 933M boundary samples (four edges at 151 points each plus a
15×15 interior grid): zero violations, worst-case slack −4.4e−16 rad (the bound
is *exactly* tight at the frustum corner when ε = 0, where Δ = β; the residual
is one fp ulp), before the sector margin. Margin is 2 sectors (0.352°) — one for
the azimuth span of a single triangle (a strip triangle spans exactly one
sector), one for the `snapped_alpha` quantization (≤ 0.5 sector) — then rounded
outward to whole 32-sector blocks (5.625° each), symmetric about the block-8
boundary, clamped to [1, 8] blocks per side. A vertex's azimuth is
`theta + snapped_alpha` in `compute_azimuth`'s convention, so block `b` maps to
`snapped_alpha - 45° + b·5.625°` and the drawn range is exactly
`snapped_alpha ± n·5.625°`. Blocks share their boundary column, so a contiguous
block range is watertight; each block ends on a primitive-restart index, so the
sub-range starts cleanly. Only the terrain draw changed: `draw_shadows` still
gets the full `index_count` at offset 0 (it needs all four 90° rotations), and
`indices_per_block` is derived as `index_count / n_index_blocks` rather than
re-deriving 68541, so the two cannot drift. Nothing else depended on the full
wedge: the POI pass runs with `depth_test` disabled and its own ±45° screen-space
filter, and the sky pass covers whatever depth the terrain no longer writes
(off-screen only). **The win is smaller than this item estimated**: the ±8.2°
figure was the horizontal half-FOV at portrait zoom 1, but the bound has to
cover the *corner* (±19.0°) because the app never removes device roll — a rolled
portrait frustum really does spread that far in azimuth. Measured block counts:
portrait 9:19.5 zoom 1 pitch 0 → 8/16 blocks (2.0x); portrait 9:16 zoom 1
pitch 0 → 8/16; the same pitched to 60° → 16/16 (no saving); landscape 16:9
zoom 0.5 pitch 0 → 14/16, pitch 60° → fallback; landscape 16:9 zoom 3 → 4/16
(4.0x); portrait aspect 0.4 zoom 0.5 pitch 0 → 14/16. So 2x at the common
portrait default and up to 4x zoomed in, but nothing at high pitch or when
zoomed out. [x] A roll-aware bound (using the actual roll instead of the corner)
would take portrait zoom 1 pitch 0 to 4/16 — worth a follow-up, but it needs
the per-edge azimuth extremum, not a cone. — DONE 2026-07-30, and the extremum
turned out *not* to be per-edge: along a frustum edge the direction is affine in
the edge parameter, so `d/dt atan2 (-d.x, d.y)` has numerator `v·u' - u·v'`,
constant, hence the azimuth is monotonic along every edge and the frustum's
azimuth extremes are attained at the four **corners** (equivalently: the
horizontal projection of a convex cone that misses the vertical axis is a salient
2D cone, whose extreme rays are among the projected generators). The earlier
"the extremum can occur mid-edge when pitched" note was wrong — the golden-section
refinement below is what settles it. So Δ is just
`max over the 4 corners of |wrap (azimuth corner - azimuth fwd)|`, computed
without any `wrap` as `atan2 (f × d, f · d)`: with `a = tx·r`, `b = ty·u` and
`h = f.x² + f.y²` the four offsets are `atan2 (±ca ± cb, h ± da ± db)` where
`ca = f.x·a.y - f.y·a.x`, `da = f.x·a.x + f.y·a.y` (same for `b`), i.e. five
products, four `atan2`s and a `max` — cheaper than the `asin` it replaced. `r`
and `u` (the view X and Y axes in world space) come from expanding columns 0/1
of `orientation`'s rotation as scalars, exactly as `Quaternion.to_matrix` writes
them, so the pass still allocates the one `fwd` record and nothing else. The
`sin β < cos ε` fallback became an *exact* containment test: the world vertical
is at view-space `(r.z, u.z, ∓f.z)`, so it is inside the frustum iff
`|r.z| ≤ |f.z|/x_scale && |u.z| ≤ |f.z|/y_scale`, in which case every azimuth is
spanned; written as `not (|r.z| > ...) && not (|u.z| > ...)` it keeps the NaN
fallback. Margin, outward block rounding, the symmetric range about block 8 and
the untouched shadow pass are all as before. Re-verified on the same grid as the
cone bound but denser (9 zooms × 13 aspects × 186 pitches × 26 rolls × 2
azimuths = 1,125,540 frusta; four edges at 401 points each plus a 21×21 interior
grid = 2.30e9 samples): **zero violations**, min slack −1.8e−14 rad, attained at
a corner (the bound is exactly tight there, as it must be). Per edge, a
201-point scan plus an 80-iteration ternary search bracketed inside [−1,1]
never beat the corner max by more than 1.8e−14 rad = one ulp — the corner claim
holds (an unclamped bracket that extrapolated 1% past a corner "found" 32° of
excess, which is what a mid-edge extremum would have looked like). The 183,024
fallback configs each got an explicit witness check: the vertical's frustum-plane
coordinates `(r.z/(tx·|f.z|), u.z/(ty·|f.z|))` lie in [−1,1]² and the direction
there has horizontal length < 1e−9 — 0 bad witnesses, so nothing is excluded
that is not genuinely degenerate. The compact `(ca, cb, da, db, h)` form agrees
with naive `atan2` azimuth differences to 1.7e−14 rad, and the scalar quaternion
expansion agrees with `transform_vector`/an explicit orthonormal basis to
1.3e−15 over 70k orientations built as yaw × pitch × roll. Δ_exact ≤ Δ_cone in
all 942,516 non-fallback configs (never draws more than b6ba4eb did); worst
Δ_cone/Δ_exact = 2.68 (zoom 3, aspect 0.4, pitch 0, roll 0). New block counts
(roll 0, cone → exact): portrait 9:19.5 zoom 1 pitch 0 → 18.99°/8 → 8.21°/**4**
(4.0x vs the old ±45°); portrait 9:16 zoom 1 pitch 0 → 19.73°/8 → 9.97°/**4**;
the same pitched to 60° → 16 → 37.47°/**14**; landscape 16:9 zoom 0.5 pitch 0 →
35.64°/14 → 32.01°/**12**, pitch 60° → cone fallback → 72.63°/16 (no change);
landscape 16:9 zoom 3 → 6.82°/4 → 5.95°/**4**; square zoom 0.5 pitch 0 →
41.47°/16 → 32.01°/**12**; portrait 0.4 zoom 0.5 pitch 0 → 33.95°/14 →
14.04°/**6**, pitch 75°/85° → fallback in both. Rolling the portrait default
degrades gracefully instead of paying the corner cost up front: roll 0 → 4/16,
10–30° → 6/16, 45–90° → 8/16 (the cone bound's constant). High pitch still
saves little and near-vertical still falls back — that part of the note stands.

Quality (cheap, ranked by value-for-effort):

- [x] Give the AO texture a mip chain + anisotropy (`viewer.ml` `create_r8_target`
      uses `tex_storage2d .. 1 ..`, LINEAR only; sampled at grazing angles every
      frame → shimmer). ~10 lines, negative per-frame cost. Best single win.
- [x] Per-cascade shadow bias (amended 2026-07-30: the constant 8 m bias
      banded slopes tilted along the light — texel rows quantize shadow_val
      when the bias is under the per-texel depth span; now 4 m + 1.5 x
      texel x slope-tangent per cascade, verified banding-free at the
      reported view and <1% drift on the reference views): `terrain.frag:339` uses flat 0.0015 NDC across
      cascades whose depth spans are 20/48/150 km → world bias 30/72/225 m
      (peter-panning + visible ring at cascade switches). Make it constant
      world-space per cascade (~8 m / span); raise normal offset to ~1 texel.
- [x] View-dependent fog color: terrain fog uses flat u_fogColor while the sky
      blends to zenith; mix by -view.z exactly as sky.frag does (bit-identical
      at/below eye line). 3–6 lines. Requires normalized view dir (below).
- [x] Banding: terrain.frag `out lowp vec4` → mediump; add post-gamma dither in
      terrain.frag; move sky.frag's dither to after the gamma pow (it currently
      dithers in linear space: 2–5x too much noise in dark blue, 2x too little
      near the sun).
- [x] Sun disc: hard `if (cos_gamma > 0.9995)` edge, 3.6° across vs the real
      0.53°; replace with a smoothstep limb at highp thresholds (mediump cannot
      represent 0.99999 — likely why the coarse threshold exists).
- [x] Renormalize v_view_dir in terrain.frag (up to 22% short within ~10 m of
      the camera; feeds half-vector/reflect/fresnel). Was already a Phase 5
      deferred item; fog-color item needs it anyway.
- [x] Look changes — DONE 2026-07-30, verified against frozen-time
      headless captures (lake reflection now brightens toward grazing;
      glacier specular restored; distant valleys ~3x less over-hazed): apply specular/env reflection after
      the lighting multiply (grazing lakes currently get sky reflection ×
      diffuse lighting → muddy); AO should attenuate ambient only, not direct
      sun (double-darkening with shadows); mean-altitude haze instead of
      fragment-altitude (~2x over-haze on distant valleys); detail-map LOD
      bias 1.0 → 0.5 — REJECTED after A/B captures (2026-07-30): the CLC
      cover-map speckle dominates surface variation, so no visible sharpness
      gain; shimmer risk and bandwidth not worth it. Bias stays 1.0.

Performance (no visible quality change; mobile/TBDR focus):

- [x] Azimuth-cull the radial grid: fixed ±45° wedge drawn vs ±8.2° needed
      (portrait zoom 1) — ~1.05M tris/frame at ~1.5 px/triangle. Index buffer
      is already block-major (16 blocks × 68541 indices contiguous), so it's
      one draw_elements offset/count per frame. Provably pixel-identical
      (±1-sector margin); shadow pass keeps the full range. Biggest win, 1.8–5x
      on vertex/binning cost. (Amended 2026-07-30: the roll-aware exact bound
      reaches the ±8.2° estimate at roll 0 — 4/16 blocks at the portrait
      default, 4.0x — where the first, roll-independent cone bound stopped at
      2.0x. See the azimuth-culling notes above.)
- [x] Skip the 5-tap PCF when final_l == 0 (both uses of shadow_val are
      multiplied by final_l) — bit-identical, removes the second-most-expensive
      fetch group on 20–40% of terrain pixels (back-facing slopes).
- [x] Clamp detail-map anisotropy to 4x (bias kept at 1.0) instead of driver max 16x (5 aniso
      fetches/fragment on band-limited noise; near-free, wants an A/B
      screenshot). NOTE: interacts with the LOD-bias quality item above —
      evaluate together.
- REJECTED — Vertex bilinear via a sampler object on the packed-RG8 height
      texture. The decode h = r + 256·g is linear, so *ideal* filtering would
      be exact (even across low-byte carries), but hardware filter output
      precision is implementation-defined and any rounding of the filtered
      g channel is amplified 256x by the decode — same failure mode as the
      Phase 3 fp16 sampler bug, metre-scale terracing. The manual
      texelFetch+fp32 mix decodes before interpolating and is immune. Only
      safe with a single-channel filterable format (e.g. R16F relief copy),
      which is a memory trade, not a free win.
- [x] Fade-gate the two side triplanar planes on blend.x/blend.y (existing
      smoothstep+neutral-0.5 pattern; error < 1/255, continuous).
- [x] Gate sky.frag's two pows on cos_gamma > 0 (pow(0,y)=0; bit-identical;
      sky is 30–60% of screen).
- [x] Split relief RGBA8 into two RG8 textures (height for vertex, normal for
      fragment): halves bytes/tap on the most heavily filtered fetch. Medium
      effort (compute_relief needs a second attachment). — DONE 2026-07-30,
      bit-identical: RMSE **0 (0)** on all three reference views (Chamonix
      α=200 β=80, Les Houches α=25 β=82, Argentière α=180 β=85, all at
      zoom 1 and frozen clock 1785398400000), rig determinism re-checked
      first (same view captured twice = 0 (0)), `gl.getError()` empty and
      mean luma 0.41 (no black frame / no shader compile failure).
      `normal.frag` was already computing both halves in one pass, so the
      split is MRT rather than an extra pass: it now declares
      `layout(location = 0) out mediump vec2 height_out` and
      `layout(location = 1) out mediump vec2 normal_out` (**mediump kept on
      both** — the old target was `out mediump vec4`, and fp16 rounding of
      `mod(h_val, 256.0) / 255.0` before the 8-bit quantization is part of
      the stored bytes; promoting to highp could flip a byte). `compute_relief`
      creates two 13-level RG8 pyramids via one `create_rg8_pyramid ~filtered`
      helper and one FBO with *both* levels attached per mip
      (`draw_buffers [ca0; ca1]`, set once — that state is per-FBO), so the
      per-level pass count is unchanged: height downsample into the RG8 scratch,
      then one normal pass that fills level N of both pyramids. The downsample
      needed a *second, single-attachment* FBO for the scratch: reusing the MRT
      FBO would leave attachment 1 (the previous level's normals) written with
      undefined values, since downsample.frag emits one output. Level N's height
      is still normal.frag's decode→re-encode of the scratch texel rather than a
      copy of the scratch itself — the roundtrip is *not* provably the identity
      (`floor(h_val/256)` can fall the wrong side after the fp round trip) and
      the scissored 1-px border differs — which is why the arithmetic was left
      character-for-character alone. The zero clear now covers both attachments,
      so the border is `(0,0)` heights + `(0,0)` normal, exactly the old
      `(0,0,0,0)` RGBA. Consumers: `radial_common.vert` untouched — `relief` is
      the height texture for both the terrain and shadow programs (unit 1,
      texelFetch `.rg`); `terrain.frag`'s sampler became `relief_normal` with
      `.ba` → `.rg`, so the fragment stage no longer declares `relief` at all
      and the §4.3.5 cross-stage type/precision question disappears instead of
      being negotiated; both samplers stay `highp`. `ao_bake.frag`/`ao_blur.frag`
      unchanged (heights, `.rg`, their NEAREST sampler object on units 0/1).
      New unit **2** for the normal texture (0, 2, 6 were free; 0 is transiently
      rebound by the detail-map/text paths, 2 is touched by nothing), uploaded
      once in `upload_texture_units`; `terrain_uniforms` gained `relief_normal`
      in both .ml and .mli. Filtering followed the data: trilinear + `LINEAR`
      mag + **driver-max** anisotropy now sit on the normal texture (the only
      `texture()` consumer, aniso deliberately *not* reduced), while heights got
      explicit `NEAREST_MIPMAP_NEAREST`/`NEAREST` — texelFetch ignores filter
      state and the AO passes override it with their sampler object, but the
      full 13-level chain is still allocated because the vertex LOD system
      fetches specific levels. Memory is exactly a wash: 4096² RGBA8 + mips =
      4·(4¹³−1)/3 = 89,478,484 B, now 2 × 2·(4¹³−1)/3 = 2 × 44,739,242 B; what
      halves is bytes/tap — 4 texelFetches per vertex in *both* the terrain and
      shadow vertex shaders, the AO bake's 81 taps/pixel, and the fragment
      normal fetch (trilinear + 16x aniso, the most expensive of the three) all
      go 4 B → 2 B. Also deleted both bake FBOs at the end of `compute_relief`
      (the old code leaked its single one; leaking two would have been worse).
      Not touched: `src/sdl`'s own inlined copies of these shaders (legacy
      client, its own RGBA8 relief), the rejected R16F variant, and the AO
      texture's recent `?limit` anisotropy work.
- [x] Small frees — DONE except: per-frame viewport set (driver no-op,
      resize-ordering risk not worth it), textureSize>>lod (needs new uniform
      plumbing for a micro win), POI call batching (real refactor, parked): center_height uploaded per frame though session-constant;
      constant clear color re-uploaded; duplicate disable(CULL_FACE); viewport
      re-set per frame; zoom-aware redraw threshold (fixed 1e-4 rad ≈ 0.35 px
      at zoom 1 → wasted settle-tail frames); textureSize(relief, lod) →
      4096 >> lod; POI pass makes ~5 GL calls per POI (~200 crossings/frame).
- [x] Startup only: shadow bake renders 3 cascades × 4 rotations × full 70 km
      grid (12.6M tris); cascade 0 only needs ~54% of rings — contiguous
      sub-ranges, provably identical.

Rejected after analysis (do not revisit without new evidence): tile
compression upgrades (measured 2026-07-31 on real tiles): DEM residues are
near the entropy floor -- zlib-9 -0.01%, zstd-19 -2.2%, brotli-11 -4.9%, and
the JPEG-LS MED predictor is 6.8% WORSE than the existing parallelogram
predictor on smooth terrain (the byte-plane split also beats combined-stream
coding); CLC gains 5.9-7.5% (concentrated in index streams; coordinate
low-byte planes are quantization noise at ~100%). A full brotli swap = ~22 MB
of 344 MB for a new worker WASM decoder + format migration; halving DEM
height precision to 0.29 m saves only ~14% at a fidelity cost. Also
previously rejected: MSAA already on
by default; detail-map anisotropy already max (the loss is the LOD bias);
cascade selection already single-lookup; water paths already branch-gated; no
discard anywhere; sky already drawn last with early-Z; shadow map already baked
once and shadow.frag already empty; remaining highp is load-bearing (v_dist
and perturbNormal's invDet overflow fp16; GGX at low roughness amplifies
mediump NdotH error); fog-based early-out impossible (fog_coeff ≥ ~0.06
everywhere); palette pre-resolve costs ~56 MB for the cheapest fetches in the
shader; tone mapping unnecessary (nothing clips but sun/specular).

## iOS orientation stability (2026-07-30) — DONE (uncommitted)

**Bug.** `handle_orientation` substituted `alpha := 360 - webkitCompassHeading`
into the W3C Euler composition while keeping the raw `beta`/`gamma`. That mixes
two reference frames and is only correct when the device is flat. Held upright
(`beta ≈ 90`, the app's primary pose) the composition is inconsistent and the
resulting view heading is `compass - gamma`: since `alpha` and `gamma` are
individually ill-defined at `beta = ±90`, the reported `gamma` wanders and the
view swings. Measured with the spec formula: at `beta = 90`, heading error is
exactly `-gamma` (0° at `gamma = 0`, -10° at `gamma = 10`, +20° at
`gamma = -20`); at `beta = 60, gamma = 30` it is -33.7°.

**Fix.** Build the quaternion from the *raw* reported triplet (self-consistent,
gyro-fused, continuous through the singularity), then rotate it about the world
vertical by the constant offset `k` between iOS's arbitrary startup frame and
the north-referenced frame. `k` is recovered per event as
`implied_compass_heading(alpha_raw, beta, gamma) - webkitCompassHeading`, where
`implied_compass_heading` is the heading formula from the spec's worked example
(`R * (0,0,-1)`, i.e. out of the back of the screen, clockwise from north).
Because both headings shift by exactly `k` under a rotation about the vertical,
the difference is `k` at *every* pose. The correction is a single left
multiplication by `Rz(k)`.

**Numerical proof** (`check_orientation.ml`, scratchpad; not part of the repo).
Grid: alpha 0..345 step 15, beta -180..175 step 5 plus ±90 and ±89.9, gamma
-90..85 step 5, k ∈ {0, 30, 123.4, 270}, screen ∈ {0, 90, 180, 270}. For each
case: `q_true` from `(alpha, beta, gamma, screen)`, `q_raw` from
`(alpha - k, beta, gamma, screen)`, delta from the two heading formulas, then
`Rz(delta) * q_raw` compared to `±q_true`.

    max |ours - spec transliteration| over non-degenerate poses: 8.527e-14 deg
    cases: 1050624, skipped (degenerate): 3840, failures (>1e-9): 0
    max quaternion component error: 8.327e-16
    max |delta - k| error: 1.137e-13 deg

Degenerate zone: `vx² + vy² = 1 - (cos beta cos gamma)²`, so the implied heading
is undefined exactly when the screen normal is vertical — the device lying flat
(`beta = gamma = 0`, or `beta = ±180, gamma = 0`). The guard rejects the whole
neighbourhood `vx² + vy² < 0.01` (~5.7° from flat), where the heading is defined
but noise-amplified. It depends on `beta`/`gamma` only, so it is the same for
both headings (verified: 0 cases where one is degenerate and the other is not).
The upright pose is maximally far from it.

**Filter.** State: `compass_delta_cos`/`compass_delta_sin`/`compass_delta_valid`
— the offset is smoothed as a vector, not an angle, so the average wraps
correctly across 360°; `atan2` on the smoothed pair needs no renormalisation.
Coefficient 0.012 ≈ 1.4 s time constant at 60 Hz. First valid sample initialises
directly. Updates are frozen (last estimate still applied, so the view stays
smooth) when `webkitCompassHeading < 0`, when `webkitCompassAccuracy` is present
and negative or > 30°, or in the degenerate zone. Before the first valid sample,
or when the event has no `alpha`, the old substitution is used as a degraded mode
— it is correct near flat, which is exactly where the offset cannot be
estimated. Simulated 10 s stream (k = 123.4, ±3° compass noise, phone raised
0→90° over 1 s then rolled ±25°): orientation error ≤ 0.71° after 2 s.

Non-iOS paths (`deviceorientationabsolute`, `absolute` flag) are untouched:
without `webkitCompassHeading` the code takes the same branch as before.

**Assumption to verify on a real iPhone.** That `webkitCompassHeading` is the
heading of the screen normal (`R * (0,0,-1)`), the same direction the spec's
`compassHeading` computes. Only then does the difference isolate `k`. The other
plausible reference is the device top (`R * (0,1,0)`), whose degenerate zone
would be the upright pose instead; switching is a one-line change in
`implied_compass_heading`. Poses/motions to check:

- Upright portrait, aimed at a known landmark: heading correct.
- Roll ±30° while upright (this is the regression: HEAD shifts the aim by
  `-gamma`, the fix must keep the aim fixed while the view rolls).
- Pitch through vertical (beta 60° → 120°): no swing, no flip.
- Full 360° azimuth turn and back: aim returns to the landmark.
- Landscape (screen angle 90 and 270): aim still correct (validates the
  screen-angle term interacting with the correction).
- Start with the phone flat on a table, then raise it: heading must snap to
  correct within ~1 s (filter initialisation), no long drift-in.
- Near a magnetic disturbance (car, speaker): the view must stay smooth, not
  jitter with the compass.

## Water reflections — designed, deferred (2026-07-31)

Goal: real mirrored terrain in lakes. Deemed practical because the camera
position is FIXED per location (only orientation varies): for a fixed eye E
and a water plane at height z, every reflected ray equals a straight ray from
the mirrored eye E' = (Ex, Ey, 2z - Ez), so the whole reflection for all
camera orientations is "what E' sees" — a static cubemap baked once per
location in load_location, like the shadow bake. Runtime cost: one cubemap
fetch per water fragment.

Key facts feeding the design:
- Multiple surface elevations per view (valley lake + tarns) are the norm, so
  one plane is not enough (user-confirmed requirement).
- No lake segmentation is needed: lakes are flat in the DEM, so a water
  fragment's own world height IS its plane height (plane_z = fragment z).
- Error of sampling a cubemap baked for plane z0 from a lake at z1: the
  mirrored eye is off by 2(z1-z0) vertically, tilting reflected content by
  ~2*dz/d for features at distance d (300 m mismatch, ridge at 3 km -> ~11
  degrees: too visible; must fall back instead).
- WebGL2 / ES 3.0 has NO cubemap arrays: K planes = K separate samplers and a
  branch. The branch is quad-coherent (all fragments of a lake share one
  height), so no divergence cost within a lake.
- ES 3.0 has no clip planes: the bake must discard fragments below the water
  plane (discard is fine in a once-per-location bake; it is only a TBDR
  problem in per-frame passes).

Plan (est. one focused agent-day):
1. Plane detection in load_location: histogram water-covered DEM heights
   (water polygons from the CLC tiles x DEM heights), weight by area and
   proximity to the camera, cluster, keep the top K=2 planes (a third only if
   its weight is substantial and its elevation differs > ~50 m from both).
   Sea coverage contributes a z=0 plane.
2. Bake per plane: 6 cubemap faces at 512^2 from E' (reuse the terrain
   pipeline; each 90-degree face maps onto the existing wedge machinery like
   draw_shadows' rotations). A uniform clip_z + discard in a bake-only shader
   variant clips geometry below the plane. Delete/rebake on location switch
   (add to delete_location). Memory ~6 MB per plane. Skip the bake entirely
   when the location has no significant water (most summits) — zero cost.
3. Shader integration (terrain.frag water path): reflectDir and the fresnel
   reflectivity already exist. Replace envColor with the cubemap sample using
   direction (fragment - E'_k), perturbed by the existing ripple/wave normal
   (wind distortion masks residual errors). Select k per fragment by
   |fragment_z - plane_k|; if the nearest mismatch exceeds ~40-60 m, fade to
   the current sky-gradient envColor — the failure mode for unmatched tarns
   is exactly today's rendering, never a wrong mirror.
4. Verification with the headless rig (frozen clock): Lac du Mont-Cenis view
   (lat=45.26029 lon=6.9329 alpha=159 beta=82 zoom=0.50) — mirrored ridges
   present and tracking across several alphas; a two-lakes-two-planes view;
   RMSE exactly 0 on water-free views; a location-switch round trip (bake
   regenerates, old cubemaps deleted); GPU memory check.

Held in reserve (exact for all planes, pricier): bake one 360-degree
color+depth panorama from the TRUE eye and ray-march it per water fragment
(static SSR against a baked G-buffer). 8-16 fetches per water pixel instead
of 1 — likely too heavy on Mali/Adreno when a lake fills the frame
(Android is the primary target).

## Near-field DEM refinement with RGE ALTI — Option B IMPLEMENTED (2026-07-31, uncommitted)

IMPLEMENTED as Option B (live IGN fetch, no hosting pipeline), with the
endpoint switched from WMS GetMap to WMTS GetTile late in the work. What
shipped, and how it differs from the design below:

- Source: `data.geopf.fr/wmts`, layer ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES,
  WGS84G level 13, FORMAT=image/x-bil;bits=32. 256 x 256 raw float32 tiles,
  0.30899 arcsec per sample (9.5 m N-S, ~3.2x the base grid and NOT a
  power-of-two ratio to it — every consumer reads `Hd_dem.px_arcsec`).
  Verified aligned with our base grid to better than a quarter of a base texel
  (shift search against the Copernicus .tif: optimum exactly at 0,0).
- Extent: an 8 x 8 tile block (2048^2 samples, 633 arcsec = 19.5 x 13.6 km at
  46N) anchored on the level-13 tile holding the location, so nearby locations
  share all 64 URLs. 214 kB deflated per tile, ~13.7 MB wire, 64 tiles in
  2.7-8.7 s in-browser (4.1 s with curl -parallel 16).
- Encoding: absolute heights, as gate 1 concluded. The enhancement-layer
  semantics live in `Hd_dem.blend`, which resamples the base tile onto the HD
  grid on the CPU and writes `base + fade * (hd - base)` into a u16 grid: the
  result *equals the base upsample* wherever HD is missing (nodata, 404 tile,
  outer 1.5 km annulus), so the renderer switches on a plain extent test and
  degrades to today's rendering by construction. 0.35-5 s for 4M samples.
- Renderer: second relief height+normal pyramid over that grid (existing
  `compute_relief`, with the texel spacing parameterised); vertex LOD, fragment
  normal, shadow bake and the POI silhouette replica all switch inside the
  extent. AO stays base-resolution (follow-up).
- `Visibility` (POI occlusion) follows the eye onto the HD surface:
  `test_precise` gained `?fine`, a finer grid consulted by the bilinear phase
  where the ray hugs the terrain, and the accessor it is handed now returns HD
  heights inside the extent, so both endpoints, the curvature frame and the
  Bresenham phase see the surface that is drawn. Without HD the call collapses
  to exactly the previous one (verified bit-identical).
- Two bugs found on first user review and fixed:
  * `compute_relief` leaves the outermost texel of every level at zero, which
    decodes as -500 m. Invisible at the base tile's rim (63 km, off-mesh); on
    the HD pyramid that rim is a few km away and rendered as a ring of
    vertical downward spikes. `compute_relief` now takes `?border`, and the HD
    bake fills its pyramid edge to edge.
  * The eye height came from the base DEM while the surface came from HD. RGE
    ALTI is +59 m against Copernicus at the Aiguille du Midi (the 30 m DSM
    smooths the summit down), so the camera ended up *inside* the terrain. The
    eye is now read off the grid that is actually drawn.
- Source caveat, not a renderer bug: RGE ALTI HIGHRES at level 13 is 32-63 m
  BELOW Copernicus on the Mont Blanc summit dome (4778 m peak vs 4810 m), so
  summit viewpoints sit lower than before. Glacier surfaces lower down are
  excellent (crevasses resolved), so this looks like the summit ice being
  surveyed at a different epoch rather than a defect.
- Verified (headless, frozen clock): grazing forest and crag views visibly
  changed (bare-earth valley floor with the Arve channel; crevasse fields on
  the Géant/Vallée Blanche glacier that the 30 m DSM cannot hold); far vista
  no seam at the boundary; POI labels still pinned at zoom 3; WMS/WMTS host
  unreachable => RMSE exactly 0 vs the pre-change baseline on all three views
  (the whole feature is a no-op without data). Terrain beyond the extent was
  bit-identical too until the eye-height fix; the eye now follows the HD
  surface, so the far field shifts with it — a whole-frame parallax shift, not
  a change of content. GPU cost +22.4 MB per location.
- Service worker: `data.geopf.fr` is cached cache-first in the persistent data
  cache, stored and matched *with* the query string (unlike the hosted tiles,
  whose query is stripped). Verified: 64 entries in cache `v1` after a load.
- Offline prefetch (added 2026-07-31): `Hd_dem.prefetch`, fired for the
  Geolocation source alongside the DEM/CLC prefetches, warms a 16 x 16 tile
  block on the same anchor (~40 MB beyond the extent), so every location
  within ~10 km keeps its full near field offline. The geoplateforme exempts
  the WMTS from its rate limiting, so all requests are issued at once, through
  the service worker (`Cache.add` would reject 404s). The worker now also
  *serves* cached 404s for these URLs (`use_cache_first ~serve_not_found`):
  a WMTS 404 means "outside French territory", which is permanent, and
  re-asking the network turned coverage-edge loads on a weak connection into
  tiles hanging until the 25 s timeout. Verified: 64 entries after the load,
  256 after the prefetch ("prefetched 192 tiles" — the inner 8 x 8 skipped).

Open follow-ups: the fetch blocks the location load (a deferred upgrade —
publish on the base tile, rebake when the tiles land — would remove the
+3-9 s); AO still uses the base grid, and `Visibility`'s Bresenham phase
samples HD at the base 1-arcsec spacing only; level 14 (0.155 arcsec) is
available at 4x the data if the near field ever needs it.

Original design notes follow.

Goal: sharper terrain within ~2-5 km of the viewer using IGN's RGE ALTI
(bare-earth LIDAR DTM, 1 m / 5 m, France, open data / Etalab licence,
Lambert-93 EPSG:2154). The radial mesh already samples every ~10 m in the
inner rings while Copernicus delivers ~30 m: the data is the near-field
bottleneck. Beyond a few km the 30 m base outresolves the mesh.

CORE REPRESENTATION (user's idea, adopted): HD tiles store the RESIDUAL
between RGE ALTI and the client-side upsample of the Copernicus base
(a Laplacian detail layer), not absolute heights. Consequences:
- Reconstruction: one GPU bake pass, bilinear(base u16 grid) + residual.
  The encoder MUST replicate the client upsample bit-exactly (same u16
  bilinear, residual quantized against it) so reconstruction is exact.
- Missing tile == zero residual == today's rendering: no fallback logic,
  border/fetch-failure/partial-hosting all degrade identically and
  gracefully. Coverage edges fade the residual to zero over a 1-2 km
  annulus.
- The DSM-vs-DTM canopy mismatch mostly dissolves: inside coverage the
  ground (RGE) is reconstructed exactly, canopy offset lives in the
  residual; at coverage edges the fade ramps ~20 m over the annulus in
  forest (~1% slope change, invisible). Footnote: forested near ridges
  silhouette at ground height instead of canopy top — a visible change at
  treelines, arguably more correct for a terrain app.
- Residuals are bounded (mostly within +-30 m): an 8-bit container at
  ~0.25 m step (with clamp or escape for outliers) halves raw size before
  entropy coding; predictor+byte-plane+gzip pipeline reusable on top.

GATE 1 RESULTS (measured 2026-07-31, Chamonix patch 6.85-6.92E/45.88-45.92N,
real RGE ALTI fetched live via IGN WMS): RESIDUAL ENCODING LOSES to absolute
encoding — 0.507 vs 0.367 byte/sample at 1/4 arcsec. Why: the parallelogram
predictor is linear, so predict(residual) = predict(RGE) - predict(upsample);
coding the residual costs everything absolute coding costs PLUS the bilinear
upsample's own prediction error (kink lines at every base-cell boundary). A
7.7 m neighbour out-predicts any 30 m base. DECISION: encode HD tiles
absolutely with the existing pipeline; keep the enhancement-layer semantics
at the TEXTURE level — reconstruction bake uses HD where present, base
upsample where absent, cross-fade annulus at coverage edges: same graceful
degradation (missing tile == today's rendering), better compression.
Hosting extrapolations: ~350 MB French Alps at 1/4 arcsec (fits alongside
the current ~380 MB artifact), ~90 MB at 1/2 arcsec (comfortable, still 2x
base resolution).

ALSO VALIDATED LIVE: data.geopf.fr/wms-r serves raw float32 elevation
(FORMAT=image/x-bil;bits=32, LAYERS=ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES,
CRS:84 bbox, WMS resamples server-side to any requested grid) with
access-control-allow-origin: * — one ~2.3 MB request covered the whole test
patch; values verified against our base (corr 0.9991; .dem sub-tile files
store row 0 = north, the south flip happens in dem_loader). A HIGHRES.MNS
surface-model layer also exists (canopy-consistent alternative). This opens
OPTION B: no pipeline, no hosting — load_location fetches near-field
elevation from IGN (~2 MB, SW-cached; Etalab licence needs attribution),
reconstruction bake identical. Remaining risks for B: IGN quotas and
availability; offline story depends on cache warmth.

REMAINING GATE: choose Option A (hosted preprocessed tiles, Corine/
pipeline, owns availability + offline) vs Option B (live IGN WMS fetch, far
less work, always current). They stage well: B as prototype, A later if IGN
dependence bothers.

IMPLEMENTATION SKETCH once gated (Option B ~1 agent-day, Option A 2-3):
1. Pipeline (Corine/): Lambert-93 projection module (follow proj_2975 /
   proj_3035 pattern); departmental-archive downloader (dep. 04, 05, 06,
   38, 73, 74); resample to the 1/4-arcsec subgrid; ABSOLUTE heights (per
   gate 1), compress with the existing predictor/byte-plane/gzip pipeline
   verbatim; fixed
   ~2.5 x 2.5 arcmin tile grid under data/dem_hd/.
2. Renderer: second smaller relief height+normal pyramid (e.g. 2048^2 over
   ~16 km) built by a reconstruction bake (HD where present, base bilinear
   where absent, cross-fade at coverage edges) then
   the existing compute_relief mip/normal machinery; vertex LOD prefers the
   HD texture inside its extent minus the fade annulus; POI silhouette
   anchor replica (rendered_height) mirrors the selection; shadow bake
   inherits via the shared sampler; AO stays base-res initially.
3. Loader: Option B: one WMS x-bil request for the ~16 km neighbourhood
   (decode float32 in the worker, quantize to u16); Option A: fetch ~9-25
   hosted tiles. Absent data falls back to base in the bake; epoch/deletion
   lifecycle as usual. SW: cache the WMS URL cache-first (immutable-ish).
4. Verification (headless rig, frozen clock): near-field grazing A/B
   (Chamonix forest view lat=45.923 lon=6.869 alpha=180 beta=85 and a
   crag-rich view); fade-annulus seam inspection incl. forest; treeline
   silhouette check (expected change); RMSE 0 beyond the HD extent; border
   view (Mont Blanc); load-time and GPU-memory (~+40 MB) budgets; POI
   anchors pinned at close range.

Interactions: curvature/projection resolution-independent; downsample
soft-max k unchanged on the HD pyramid; visibility could later use HD for
near occlusion (minor).

## Verified clean (no action)

Triplanar blending weights, shadow-cascade selection and
`sampler2DArrayShadow` layout, clipmap-level formula, vertex/fragment texel
conventions, varying precision matches across all linked stages, uniform
bindings vs `render_state.ml`, `manifest.json`, `dune` rules.

## Verification

- `dune build src/web` after each phase; `dune build @fmt` before committing.
- Phase 1: throttle/offline in devtools → tile failures must surface or
  degrade, never hang; kill a worker mid-load → error propagates.
- Phase 2: deploy twice with a worker-only change → returning client gets the
  new worker; fill cache quota → tiles still load; first visit → offline
  relaunch works; test http:// LAN origin and Firefox private browsing.
- Phase 3: visual check on an actual iOS device (sun disc, AO banding,
  terrain jitter at altitude).
- Phase 4: `?lat=-21.2&lon=55.5` position accuracy; `?alpha=nan` must be
  rejected; frame-time/GC profile with many POIs.

## In-page location switching (2026-07-30) — DONE (uncommitted)

**Problem.** Every location change went through `navigate_to`, i.e. a full page
reload: the WebGL context, all shader programs, the clipmap index buffer
(1.1 M indices), the detail-map KTX2 fetch, the service-worker handshake and the
event listeners were all rebuilt to move 100 km. Nothing in the renderer was
actually per-session; only the DEM/CLC-derived state was per-location.

**Split.** `tri` was doing one-time setup and per-location baking in one
non-returning function. It is now:

- `run_renderer` (one-time): CLC palette texture, scratch matrices/bigarrays,
  the first `load_location`, `start ()`, overlay hide, installing the
  `switch_location` hook, then `event_loop`. Programs, geometry, detail map,
  shadow map + FBO, samplers and uniform locations were already one-time in
  `init_graphics`/`main` and stay there.
- `load_location ~lat ~lon` (repeatable): DEM + CLC fetch, POI extraction
  (`poi_positions`, moved out of `main`), centre height, then the bakes — tile
  texture, relief height/normal pyramids, AO, `upload_session_static` (deltas
  and `center_offset` depend on `lat`, `light_dir` is recomputed from
  `Sun.position ~lat ~lon`), CLC cover map, shadow matrices + `draw_shadows`
  into the *same* 2048²×3 shadow map, `bind_terrain_textures`, `center_height`,
  clear colour, POI visibility filtering and sort.

**Location record.** All per-location state moved into an immutable `location`
record (`x`, `y`, `height`, `lat`, `lon`, `tile`, `points`, `tile_texture`,
`relief_texture`, `relief_normal_texture`, `ao_texture`, `cover_map_texture`)
held in `session : location option ref`. `draw` takes `~location` and opens it
with a record pattern — plain field reads, no allocation in the render loop —
which also let ten dead parameters (`_shadow_matrices`, `_ao_texture`,
`_palette_texture`, `_cover_map_texture`, `_tile_texture`, `_relief_texture`,
`_w`, `_detail_map`, `_shadow_pid`/`_fbo`/`_map`/`_uniforms`) go. Publishing is
a single `ref` assignment, so a frame sees one whole location or the other.

**Failure and concurrency discipline.** The network comes first and nothing is
touched until both fetches have succeeded: a failing or superseded load leaves
the previous location rendering. `location_epoch` is bumped at entry and
re-checked after the only await; a stale load returns without deleting or baking
anything (Lwt cancellation is awkward, epoch checks are not). Between the epoch
check and the `session` assignment there is no await, so no frame can observe a
half-replaced location. GPU order: fetch → epoch check → delete the previous
location's five textures *and its lazily created POI text textures* → bake →
publish. Deleting first keeps the peak at one location's worth of DEM-sized
textures; the bakes rebind every unit the dead textures occupied (verified: no
GL errors).

**Camera.** `current_orientation`, `input_mode` and `zoom` deliberately survive
a switch — heading continuity is the point. The URL is rewritten via
`update_url_params` after a successful switch so reload/share land on the new
place.

**Startup status.** `main` announced "Getting current location..." before
resolving the position, so a `?lat=&lon=` visitor saw it during the
service-worker wait and the tile fetches. The message now lives in the `None`
branch of `get_position`, the only one that queries the device; everything else
shows "Loading Terrain..." from the start.

**Still reloading (by design).** The Featured list (`navigate_to`, carries
alpha/beta/zoom presets) and `main`'s startup redirect when URL parameters were
present but unusable. The coordinate input and "Use My Location" switch in
place.

## Verification (2026-07-30)

`dune build src/web` and `dune build @src/web/fmt` clean (run in a private
`--build-dir`; the project watcher held the default lock). No GLSL change.

Headless-Chrome captures, 1200×800, `Date.now` frozen at 1785398400000 so the
sun is deterministic; A = Chamonix `45.878,6.887`, B = Ventoux `44.174,5.279`;
`compare -metric RMSE`:

| Case | Result |
| --- | --- |
| Load A (with B's angles) → switch to B, vs direct B | `0 (0)` |
| A → B → back to A, vs direct A | `0 (0)` |
| A → B, superseded by A-coords before the overlay clears, vs direct A | `0 (0)` |
| A → B → Réunion → Col Girardin → A, vs direct A | `0 (0)` |
| Réunion (`-21.115,55.48`) cross-dataset switch | renders, mean 0.277, POIs placed |

Pixel-exactness across a switch also covers the POI text atlas (the shared text
canvas grows for the previous location's labels) and the shadow/AO/cover-map
rebakes. Browser log after every switch: no GL errors, only the pre-existing
"GPU stall due to ReadPixels" performance warning from `on_gpu_finished`.
Rejected inputs (`0,0` out of range, `1e999,5`, free text) leave the scene, the
URL and the open menu untouched.

Overlay wording, captured with a `MutationObserver` on `#status-text` (a poll
misses the flash): with URL parameters `["", "Loading Terrain..."]` — the
geolocation wording never appears, including across an in-page switch; without
them `["", "Loading Terrain...", "Getting current location...", "Loading
Terrain..."]`.

## Nested level-14 near field — SUPERSEDED design notes (2026-07-31)

See "Nested near field — SHIPPED" at the end of this file for what was actually
built. Kept because the measurements and the rejected alternatives still hold.

Follow-up to "level 14 (0.155 arcsec) is available at 4x the data if the near
field ever needs it" above. The proposal is to **nest** L14 inside the existing
L13 block rather than replace it: base -> L13 -> L14, each layer fading into the
previous one.

### What the service actually offers (measured 2026-07-31)

`GetCapabilities` on `data.geopf.fr/wmts`, parsed per layer:

| layer | TileMatrixSet | levels | finest | format |
| --- | --- | --- | --- | --- |
| `ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES` (RGE ALTI DTM) | `WGS84G_6_14` | 6-14 | 4.77 m | BIL f32 |
| `…HIGHRES.MNS` (surface model) | `WGS84G_4_17` | 4-17 | 0.60 m | BIL f32 |
| `…HIGHRES.QUALITY` (source mask) | `PM_6_16` | 6-16 | — | PNG |
| `IGNF_LIDAR-HD_MNT/MNS/MNH_….SHADOW` | `PM_0_18` | 0-18 | — | PNG hillshade |

**L14 is the floor for bare earth over WMTS** — but not over WMS, see the
section below. The generic `WGS84G` matrix set runs to L19 (0.15 m) and the URL
we send names it, but the HIGHRES *layer* binds only levels 6-14 of it;
`TILEMATRIX=15` 404s over the Ubaye at two different tiles while 13 and 14 both
return full 256 kB tiles. Anything deeper on *WMTS* is a surface model or a
hillshade. The `wms-r` endpoint is not bound to the tile matrix at all and does
serve finer bare earth; L14 remains the right step for the nested layer planned
here, but as a bandwidth choice rather than a data limit.

Exact spacings from the matrix definitions (`MatrixWidth` x `TileWidth`), at 46N:

| level | arcsec/sample | m N-S | m E-W |
| --- | --- | --- | --- |
| 13 (current) | 0.3090 | 9.54 | 6.63 |
| 14 | 0.1545 | 4.77 | 3.31 |

### Why it is worth doing (measured, one tile)

L14 tile `row=4150 col=17002` against its parent L13 tile `row=2075 col=8501`
(Ubaye, 44.40N 6.80E, 572 m of relief in-tile):

- 2x2 box average of L14 vs L13: **RMS 0.000 m, max 0.00 m**. L13 *is* the box
  mean of L14 — so L14 is the genuine source and L13 a derived pyramid level.
- L14 vs a bilinear upsample of L13 (roughly what the shader reconstructs
  today): **RMS 0.365 m, peak 6.63 m**. That is the real information gain.
- Detail beyond L14's own 2x2 mean, i.e. what L13 cannot represent at all:
  RMS 1.315 m, peak 16.40 m.

The first result matters for the design: because the two layers agree exactly
under box-averaging, fading L14 into L13 is fading onto a *consistent* surface,
unlike fading L13 into the 30 m Copernicus base. The step to hide is a few
metres, not tens, so the L14 annulus can be far narrower than 1500 m.

Visibility check (60 deg FOV / 1080 px = 0.970 mrad/px): 1 px is 0.97 m at 1 km
and 4.85 m at 5 km. So L13's 9.54 m is ~10 px at 1 km (visibly coarse) and ~2 px
at 5 km (at the floor). The gain is concentrated inside ~2 km, which sets the
extent below.

### Sizing

Block sizes stay powers of two (the relief pyramid wants it). Per-location wire
cost at 214 kB/tile:

| L14 block | samples | extent (N-S x E-W) | half-extent | wire | GPU |
| --- | --- | --- | --- | --- | --- |
| 4x4 = 16 tiles | 1024^2 | 4.9 x 3.4 km | 2.44 x 1.70 km | +3.4 MB | **+5.33 MiB** |
| 8x8 = 64 tiles | 2048^2 | 9.8 x 6.8 km | 4.88 x 3.39 km | +13.7 MB | +21.33 MiB |

GPU is two RG8 pyramids with the full mip chain, i.e. `2 * 4/3 * size^2 * 2`
bytes — computed, not scaled. Against a 140 MiB per-location baseline the 4x4
block is under 4%.

**Start with 4x4.** It is +25% on the wire against today's 13.7 MB, covers the
~2 km where L14 is visible, and costs almost nothing in GPU memory. 8x8 doubles
the wire and adds a second copy of the current HD cost, for extent that is
already near the visibility floor.

`fade_metres` must become per-layer: 1500 m would consume almost all of a
1.70 km E-W half-extent. Propose **300 m** for L14 — hiding a 6.6 m peak step
over 300 m is a 2.2% slope change, invisible — leaving ~2.1 x 1.4 km of
full-resolution core.

### Phases

**L14-0 — gates, before any code.**
- Measure the L14-vs-bilinear-L13 residual at the actual featured locations, not
  just the one Ubaye tile. Gate: if it is below ~0.2 m RMS there, stop.
- Probe L14 coverage breadth across the data footprint (Alps, Provence, Corsica,
  Réunion). Patchiness is fine — the nodata/fade path handles it — but it should
  be known, since Réunion in particular may be absent from RGE ALTI HIGHRES.
- Time 16 L14 tiles in-browser, to confirm the added latency against
  `hd_grace_s`.

**L14-1 — parameterise `Hd_dem` by level. Pure refactor, ship alone.**
`matrix_level`, `tiles_per_axis`, `px_arcsec`, `block_tiles`, `size` and
`fade_metres` are module constants today, and `Hd_dem.size` / `Hd_dem.px_arcsec`
are read at ~20 sites in `viewer.ml`.
- Add `type layer = { matrix_level; tiles_per_axis; block_tiles; px_arcsec;
  size; fade_metres }` with `val l13 : layer` and `val l14 : layer`; thread it
  through `tile_url`, `anchor_tile`, `fetch`, `blend`, `prefetch`.
- `type t` carries its `layer`, so consumers read `g.layer.size` instead of the
  module constant. This is the mechanical bulk of the work.
- Gate: RMSE exactly 0 against the pinned views. It must be a no-op.

**L14-2 — generalise `blend` to chain onto an arbitrary grid.**
`blend ~lat ~base raw` hardcodes the base as 1-arcsec-spaced and anchored at
`bsize / 2` (`let half = float (bsize / 2)`, `col_of j = clamp_base (half +.
raw.origin_x +. float j *. px_arcsec)`).
- Introduce `type source = { grid : Dem_loader.t; origin_x; origin_y;
  px_arcsec }`. The base tile is `{ grid = tile; origin_x = -.(float (w / 2));
  origin_y = same; px_arcsec = 1.0 }`; an `Hd_dem.t` is already almost one, so
  chaining is natural — `blend` writes a `Dem_loader.t` and reads u16 pairs
  either way.
- Rewrite `col_of` / `cby` in terms of the source's origin and spacing.
- Gate: L13-onto-base must be bit-identical to L14-1.

**L14-3 — the second layer, end to end.**
- `viewer.ml`: two fetches; `publish ~hd_grid` becomes `publish ~hd13 ~hd14`.
  Both in the `hd_grace_s` window. A late L14 is a *good* candidate for the
  refinement path — the eye-height pop it causes is bounded by the L13<->L14
  difference (0.365 m RMS, 6.6 m peak) rather than the ~60 m base<->L13 gap that
  motivated the grace period in the first place.
- Blend chain `base -> L13 -> L14`. If L13 is missing but L14 present, chain L14
  straight onto the base; the fade machinery already covers it.
- Bake: a third `compute_relief ~spacing_scale:l14.px_arcsec ~border:false` at
  1024^2. `hd_relief` grows to two texture pairs (or an array); `delete_location`
  and the refinement-supersede cleanup in `publish` follow.
- Shaders. `radial_common.vert` has `hd_valid / hd_relief / hd_scale / hd_bias /
  hd_lod_bias / hd_max_lod` and `insideHd`; `computeRadialVertex` picks HD or
  base at line ~117. Add the `hd2_*` set and test innermost-first:
  `if (insideHd2(...)) else if (insideHd(...)) else base`. `terrain.frag` mirrors
  it at line ~383 for the normal; `terrain_main.vert` needs a second varying and
  a second `hd_half_texel`. The shadow pass shares the include, so it should
  follow — verify rather than assume.
  **Check the varying and sampler budget on Mali/Adreno** before committing to
  two extra of each.
- `hd_at` / `ray_height` / `fine` (POI occlusion) and the `rendered_height`
  selection for label anchoring both become innermost-first, with
  `inv_avg_delta /. layer.px_arcsec` per layer. Eye height reads the finest grid
  that covers the anchor.
- **Do not prefetch L14 initially.** 2x a 4x4 block is 64 L14 tiles (13.7 MB)
  for only ~2.4 x 1.7 km of offline roaming, against the L13 prefetch's ~10 km
  for ~40 MB. Leaving L14 online-only keeps the offline story exactly as it is,
  and its absence offline is invisible by construction.
- Service worker: no change, `is_hd_dem` matches on host.

**L14-4 — quantisation gate.**
`u16_of_metres` spreads 9500 m over 65536 steps: a **14.5 cm** quantum. Per unit
of sample spacing that is 0.87 deg of normal quantisation at L13 and **1.74 deg
at L14** — against a measured gain of 0.365 m RMS, i.e. **2.5 LSB**. A good part
of what L14 buys would arrive as terracing in the normals.
- Gate: bake L14 with the current absolute u16 and look for terracing on a
  shaded near-field view on a real Mali/Adreno device. Banding on fp16 mobile
  GPUs already bit once (062d383).
- **Fix: per-layer scale and offset, not a wider format and not residuals.** The
  16 bits are not the problem; spending them on a 9500 m range that no single
  grid contains is. The L14 tile measured above spans **522 m** of height, so
  rescaling u16 to a grid's own min/max recovers 18x the precision for zero
  memory, zero bandwidth and zero extra texture taps.

  Precision matters for *normals*, not heights: the quantum is divided by the
  sample spacing when differenced. Taking today's L13 as the acceptance bar
  (0.87 deg, and it ships):

  | spacing | global u16 (14.5 cm) | u16 over the grid's own range |
  | --- | --- | --- |
  | L13 9.54 m | 0.87 deg (the bar) | 0.125 deg |
  | L14 4.77 m | 1.74 deg | 0.096 deg (522 m range) |
  | WMS 1.19 m | 6.95 deg | 0.384 deg |
  | WMS 0.60 m | 13.58 deg | 0.729 deg |

  Even pessimistically — 1500 m of relief inside a 1 km box — a 1 m layer lands
  at 1.10 deg, still 6x better than the 6.95 deg the global scale gives.

  This **supersedes the residual encoding** proposed earlier (see "CORE
  REPRESENTATION" above). Residuals reach the same precision but make the shader
  sample coarse *and* fine and add them, coupling the fine layer's
  reconstruction to the coarse layer's interpolation — the objection that killed
  it at gate 1. Per-layer scaling gets the precision without either.

  Scope: `Dem_loader.t` gains `scale`/`offset` and `get_height` uses them (also
  needed by the CPU-side POI anchoring and `Visibility`); `Hd_dem.blend` emits
  them, and it already scans the grid for nodata so min/max is free; and
  `common_header.frag`, `normal.frag`, `downsample.frag`,
  `radial_common.vert` stop hardcoding `HEIGHT_SCALE` and `-500.0`. Base tiles
  keep today's constants, so they stay bit-identical and the `.dem` pipeline and
  wasm decoder are untouched. At a layer boundary the two grids then agree only
  to the coarser one's quantum, ~3 cm — invisible.
- Fallback if some layer's own range is still too wide: **R32F** heights, 2x the
  memory (10.67 vs 5.33 MiB for a 1024^2 pair) and needs
  `EXT_color_buffer_float`. Viable because heights are only ever read by
  `texelFetch` or a NEAREST sampler (see the comment at `viewer.ml:931`), so
  `OES_texture_float_linear` is not required — only the normal texture is
  filtered.

### Rejected

- **Replace L13 with L14** instead of nesting. At constant tile budget the
  extent halves (19.5 -> 9.8 km) and the offline roaming range halves with it.
  The mid field at 5-8 km still draws from the HD pyramid, and the prefetch
  range is the more valuable property.
- **`…HIGHRES.MNS` at L15-L17 as a way past the DTM's L14 floor.** Rejected on
  measurement, not on principle. Both layers serve L14 on `WGS84G`, so they can
  be differenced pixel-for-pixel; on the Ubaye tile (`L14 row=4150 col=17002`,
  2017-2539 m):
  * Where there is no canopy the two products agree to **centimetres** — median
    MNS - DTM = +0.056 m, 46.3% of samples inside one u16 LSB (0.145 m), 80.7%
    within 1 m. No systematic epoch or processing offset: on bare ground they
    are interchangeable.
  * The tile is *not* uniformly bare despite its altitude — 14.7% of samples
    differ by more than 3 m, up to 24.7 m (larch/pine on the lower shaded
    slopes). It straddles the tree line, so "high viewpoint => bare" is not a
    usable assumption. An earlier note here claiming MNS == DTM above the tree
    line was wrong.
  * MNS L15 is a genuine box pyramid of MNS L14 (2x2 mean matches to RMS
    0.0001 m), but the detail it adds **over bare ground** is only 0.169 m RMS
    against a bilinear upsample of L14 — barely above the 0.145 m u16 quantum,
    and less than half the 0.365 m that DTM L13 -> L14 buys. Over vegetated
    ground the same step adds 0.757 m RMS, 4.5x more.
  * Conclusion: the MNS's extra levels exist mostly to resolve canopy and roofs
    — the very thing a mask would exclude — so the MNS is not the way past L14.
    NOTE (corrected below): the stronger claim first written here, that there is
    no bare-earth signal left below ~5 m, was wrong. It generalised from an MNS
    pyramid step, and the `wms-r` measurements in the next section show 0.288 m
    RMS of real bare-earth detail between 4.77 m and 1.19 m.
  * Using the CLC cover map as the bare/vegetated mask (it is already loaded,
    co-registered and on the GPU via `rasterize_clc_tiles`) would work — the
    difference distribution is bimodal enough to classify — but it is moot given
    the above: 4x the tiles to recover ~17 cm RMS the u16 grid can barely hold.
  * Worth keeping in mind for a *different* feature: `MNS - DTM` at L14 is a
    canopy height model, exactly co-registered, at 4.77 m. That is the missing
    input for the known artefact noted in the original design above — forested
    near ridges silhouette at ground height instead of canopy top. CLC gives the
    class but not the height.
- **The `SHADOW` layers (PM, to L18) as an elevation source.** Not a format
  objection — PNG is lossless and elevation-in-PNG is standard practice (Mapzen
  Terrarium: `R*256 + G + B/256 - 32768`, 3.9 mm steps, finer than our u16's
  14.5 cm). The blocker is that they are *estompage*: baked shaded relief.
  Decoded, the tiles are 8-bit **grey+alpha**, spanning 0-254 within a single
  256x256 tile with mean neighbour |delta| 8 and peaks of 162 — a shading image,
  not a height field. Hillshade ~ f(normal . fixed light) collapses both normal
  components onto one scalar and drops absolute elevation, so it is not
  invertible even in principle: a perfect inversion yields slope with sign
  ambiguity along the light azimuth. `…HIGHRES.QUALITY` is likewise a *source*
  mask (which survey produced each cell), not vegetation height, and answers
  400/404 at PM L16/L18 anyway.
  If a data-bearing PNG layer ever does appear, the web-platform traps are:
  canvas readback truncates to 8 bits/channel (16-bit greyscale loses half its
  precision through `getImageData` — upload straight to a WebGL texture and
  decode in the shader instead); `drawImage`+`getImageData` round-trips through
  premultiplied alpha and corrupts any tile whose alpha is not 255 (use
  `createImageBitmap(blob, {premultiplyAlpha:'none',
  colorSpaceConversion:'none'})`); and an `iCCP`/`gAMA` chunk can shift values —
  these tiles carry neither, their chunks are just IHDR/IDAT/IEND.
- **Non-power-of-two blocks** (6x6 = 1536 samples): the relief pyramid wants a
  power of two.

### Verification

- L14-1 and L14-2 are refactors: RMSE exactly 0 on the pinned views, or they are
  wrong.
- With L14 on, expect change *only* inside the L14 extent. Check the L13/L14
  boundary at grazing incidence for a seam, and the L14 rim for the zero-border
  spike ring that `?border` was added to fix at L13.
- Frozen-clock captures per the headless rig; GPU budget per location against
  the 140 MiB baseline below.
- A host-unreachable run must still be RMSE 0 against the pre-change baseline,
  as it was for L13.

## GPU texture memory audit + two changes (2026-07-31, uncommitted)

Read off the allocation calls (formats, dimensions, level counts), not estimated.
MiB = 2^20. `w = h = 4096` (`tile_width`), HD grid 2048, cover map 7 clipmap
levels, shadow map 2048^2 x 3.

Per location, before -> after the two changes below:

| texture | format | size | levels | MiB before | MiB after |
| --- | --- | --- | --- | --- | --- |
| `relief_texture` | RG8 | 4096^2 | 13 | 42.67 | 42.67 |
| `relief_normal_texture` | RG8 | 4096^2 | 13 | 42.67 | 42.67 |
| `tile_texture` | RG8 | 4096^2 | 1 | 32.00 | **0** |
| `cover_map_texture` | R8UI array | 1024^2/2048^2 x 7 | 1 | 7.00 | **28.00** |
| `hd_relief_texture` | RG8 | 2048^2 | 12 | 10.67 | 10.67 |
| `hd_relief_normal_texture` | RG8 | 2048^2 | 12 | 10.67 | 10.67 |
| `ao_texture` | R8 | 2048^2 | 12 | 5.33 | 5.33 |
| | | | | **151.00** | **140.00** |

Session-wide, unchanged: `shadow_map` DEPTH_COMPONENT24 2048^2 x 3 = 36 MiB, or
48 MiB where the driver pads to D24X8; `detail_map` 1024^2 compressed, 11 levels
= 1.33 MiB; `palette_texture` 512 B; `hd_placeholder` 2 B. Plus the POI labels —
one RGBA8 per *visible* label, sized from a 48 px font measurement in
`prepare_text_immediate`, created lazily by `draw_text` and freed with the
location; ~70 KiB each, so a few MiB on a busy panorama. That is the one item
not statically knowable.

Resident 200.33 -> 189.33 MiB. Peak during a load 266.33 -> 249.33 MiB.

**Change 1: free `tile_texture` after the relief bake.** Its only GPU use was
`Gl.bind_texture` inside `compute_relief`; it is not among the textures
`bind_terrain_textures` binds and nothing samples it per frame — it sat in the
`location` record purely to be deleted later. Now deleted immediately after the
bake, exactly as the HD path already did with its own source grid ("Nothing
reads the source grid on the GPU after the bake"). The field is gone from
`type location` and `delete_location`. **-32 MiB per location.**

**Change 2: `cover_map_size` 1024 -> 2048.** A clipmap level spans half the
ground of the one above at the same texel count, so a cover-map texel subtends a
*constant* angle wherever the viewer stands: 1/512 to 1/256 of the distance,
i.e. 2-4 screen pixels at 60 deg / 1080 px. The whole clipmap was therefore
uniformly ~4x coarser than pixel scale, and material boundaries showed as
stair-steps of that width at every distance — which is also why adding an inner
level would not have helped, and why raising the resolution is the right fix.
2048 gives 1-2 px per texel. `sampleCLCBilinear` reads the size from
`textureSize` and the count from `u_numLevels`, so no shader change.
**+21 MiB per location**, and 4x the rasterisation fill.

**Enabler for change 2: one CLC depth renderbuffer instead of seven.** The
render loop is level-outer and clears depth at the top of every level
(`Gl.clear ctx Gl.depth_buffer_bit`), so only one depth buffer is ever live; the
old comment about needing one per level "to prevent conflicts when batching
tiles" described an ordering the code does not use. At 2048 the seven would have
been 56 MiB of transient against 8 MiB. This is what keeps the peak from rising:
the largest transient set goes 66 -> 60 MiB despite the resolution doubling.

Net: change 1 more than pays for change 2 — 11 MiB less resident and 17 MiB
lower peak, with a sharper land-cover map.

NOT VERIFIED on a device. What to check: the cover map at 2048 is a 4x fill
increase in `rasterize_clc_tiles`, so time that bake on Mali/Adreno before
assuming it is free; and confirm visually that land-cover transitions lose their
staircase. `dune build src/web` and `dune build @fmt` are clean.

## LIDAR HD bare earth at 1 m over WMS — SUPERSEDED design notes (2026-07-31)

See "Nested near field — SHIPPED" at the end of this file. The source survey,
the cost figures and the rejected alternatives here still hold; the chain and the
phase list do not.

`data.geopf.fr/wms-r/wms` (user's pointer) serves bare-earth DTM at arbitrary
resolution via GetMap, so the L14 ceiling in the section above is a WMTS
packaging limit, not a data limit. Layer
`IGNF_LIDAR-HD_MNT_ELEVATION.MIXED.WGS84G`, `FORMAT=image/x-bil;bits=32`,
`CRS=EPSG:4326` (BBOX is lat,lon in WMS 1.3.0), `MaxWidth/MaxHeight = 5010`.
Rate limit 50 req/s.

### Measured (same footprint as WMTS L14 `row=4150 col=17002`, Ubaye)

| step | residual RMS | p99 | max |
| --- | --- | --- | --- |
| DTM L13 -> L14 (4.77 m), reference | 0.365 m | — | — |
| **L14 -> 1.19 m** (1024 px over the tile) | **0.288 m** | 1.05 m | 9.07 m |
| 1.19 m -> 0.60 m (2048 px) | 0.109 m | 0.32 m | — |
| u16 quantum | 0.145 m | | |

- 1 m buys 79% of the L13 -> L14 gain. Real, worth having.
- **0.5 m is not worth having**: 0.109 m RMS is below the u16 quantum and p99 is
  0.32 m, for 4x the bytes. Even with the residual encoding it stays marginal.
  1 m is the floor that matters.
- 4x4 box-mean of the 1 m data vs WMTS L14 is 0.098 m RMS but max 3.14 m, so
  RGE ALTI HIGHRES is *not* a decimation of LIDAR HD. They are related products,
  not nested levels — a 1 m layer will not fade into L14 as cleanly as L14 fades
  into L13 (where the box-mean matched to RMS 0.000).
- `…MNT_ELEVATION.ELEVATIONGRIDCOVERAGE.WGS84G` and `…MNT_ELEVATION.MIXED.WGS84G`
  are byte-identical here. Prefer MIXED if it falls back to RGE ALTI outside
  LIDAR HD coverage — UNVERIFIED, test on an unflown area, since the flight
  programme is incomplete.

### Cost

**`wms-r` does not compress.** A 1024^2 request is exactly 4,194,304 B on the
wire; `image/geotiff` and `image/tiff` return the same size plus headers. Local
`gzip -9` reaches 3.19 MB, so the server simply does not set `Content-Encoding`
and nothing client-side can recover it. WMTS deflates to 79% by comparison.
Measured 0.75-1.7 s per 1024^2 request (the 0.5 s figure is presumably for
smaller ones).

| layer extent | requests | wire | GPU | half-extent |
| --- | --- | --- | --- | --- |
| 1 L14 footprint, 1024^2 @ 1.19 m | 1 | 4.0 MB | 5.33 MiB | 610 x 436 m |
| 2x2 footprints, 2048^2 @ 1.19 m | 4 | 16.8 MB | 21.33 MiB | 1.22 x 0.87 km |

The 2x2 option costs more wire than the entire current 64-tile L13 block.

### Design

**Quantise the BBOX to the WMTS L14 tile grid** — the probe above used exactly a
tile footprint. This is what keeps everything else working: the URL becomes a
deterministic function of (row, col), so `anchor_tile` carries over, nearby
locations share URLs, and `is_hd_dem` already matches on the
`https://data.geopf.fr/` host prefix and caches with the query string included.
**The service worker needs no change.** An arbitrary per-location bbox would
destroy the cache-first rule and the offline story with it.

- **Use the 2x2 block, not one footprint** — anchored on the tile *corner*
  nearest the user, so the user is always at least 610 m N-S / 436 m E-W from
  any edge. A single footprint is anchored on the tile *holding* the location
  (as the L13 block is), which is harmless over 19.5 km but not over 1.22 km:
  the user can sit anywhere in it, average nearest-edge distance ~218 m, so the
  layer would often reach only a couple of hundred metres in the direction being
  looked at. Each request is still a whole grid-aligned footprint, so URLs stay
  deterministic and shared between nearby locations. This forces the 16.8 MB /
  4-request option rather than the 4 MB one.
- Deliver it through the refinement path (`publish ~hd_grid`, the two-phase load)
  so its latency never blocks first paint. It is the ideal refinement candidate:
  detail-only, invisible in its absence.
- **Do not prefetch for offline.** 4 MB per ~600 m of roaming against the L13
  block's ~40 MB for ~10 km is the wrong trade.
- Angular check: 1.19 m matches pixel scale at ~1.2 km, so a 610 m half-extent
  sits inside the useful range with L14 taking over beyond it.
- Because L14 and 1 m are not box-consistent (max 3.14 m above), this layer needs
  a real fade annulus, unlike the narrow one proposed for L14-onto-L13.

### Rejected: eye-centred encoding window with height fallback

Idea: instead of scaling u16 over a grid's own min/max, centre a fixed window on
the *viewer's* elevation and fall back to a coarser layer where a sample falls
outside it. Bounded worst case instead of a data-dependent one. Measured on the
1 m layer over one L14 footprint at four featured locations, bar 0.87 deg:

| location | relief | min/max q | min/max | +-256 m | +-512 m |
| --- | --- | --- | --- | --- | --- |
| Col Girardin | 680 m | 10.4 mm | **0.498 deg** | 0.376 deg, 96.5% | 0.751 deg, 100% |
| Aiguille du Brévent | 941 m | 14.4 mm | **0.690 deg** | 0.376 deg, **27.4%** | 0.751 deg, 66.2% |
| Plateau d'Emparis | 661 m | 10.1 mm | **0.484 deg** | 0.376 deg, 84.1% | 0.751 deg, 99.3% |
| Pic de Morgon | 800 m | 12.2 mm | **0.587 deg** | 0.376 deg, 65.1% | 0.751 deg, 88.1% |

(percentages are the share of the footprint the fine layer could still serve)

- **min/max already clears the bar everywhere, at full coverage.** The 1500 m
  worst case feared in L14-4 above does not occur; the worst real footprint is
  Brévent at 941 m.
- The window is *dominated*. At Brévent, +-512 m is both less precise (0.751 vs
  0.690 deg) and covers only 66%, even though its 1024 m span exceeds the 941 m
  of relief — the eye sits near the top of the box, so a symmetric window spends
  half its range on sky. min/max is by construction the tightest window that
  still covers everything, so any eye-centred window is either wider (worse
  precision, no coverage gained) or narrower (better precision, coverage lost).
- Rendering objection independent of the numbers: a height-based fallback
  boundary is a *contour line*, and the fine and coarse layers differ by up to
  3.14 m (measured above), so the switch would draw a horizontal seam across the
  slope — the most conspicuous possible orientation. The extent boundary by
  contrast is a distant square edge that `blend` fades to exact agreement.

The spatial half of the same idea *was* right and is folded into the design
above: anchor the block on the nearest tile corner so the user is never near an
edge.

### Revision: 2.38 m, not 1.19 m (measured over the four featured locations)

Wire cost tracks *sample count*, not resolution — a 1024^2 GetMap is 4.0 MB
whether it covers 1.2 km or 2.4 km — so a coarser grid buys proportionally more
ground for the same bytes and the same GPU. Combined with where the detail
actually is, that inverts the earlier recommendation.

First, the server's 512 px output is **exactly** the 2x2 box mean of its 1024 px
output (RMS 0.0001 m). Two consequences: the residuals below are exact rather
than proxies, and **any two resolutions drawn from the same WMS layer are
box-consistent**, so they fade into each other with no discrepancy — the property
L13/L14 have and that RGE ALTI vs LIDAR HD does not (3.14 m max, above).

| step | RMS | p99 |
| --- | --- | --- |
| 4.77 -> **2.38 m** | **0.693 m** | 2.93 m |
| 2.38 -> 1.19 m | 0.423 m | 1.70 m |

The 2.38 m step captures the larger share of the refinement. At equal ground
(+-1.22 km): 2.38 m is 1024^2 = 1 request, 4.0 MB, 5.33 MiB; 1.19 m is 2048^2 =
4 requests, 16.0 MB, 21.33 MiB. The 2.38 m grid also fixes the centring problem
by itself — +-1.22 km with corner anchoring gives the same >=610 m margin the
2x2 1 m block needed 16.8 MB to reach — and it is pixel-matched to 2.46 km while
covering +-1.22 km, i.e. finer than pixel scale everywhere inside its own extent.

### Revision 2: use RGE ALTI over WMS, not LIDAR HD (user's proposal, measured)

Two measurements settle the source question, both against LIDAR HD.

**Water is not rectified in LIDAR HD.** Per-sample neighbour differences over
open water at 1.19 m, converted to normal tilt (`atan(|dh| / spacing)`):

| product | mean tilt | p99 | max |
| --- | --- | --- | --- |
| LIDAR HD MNT | **0.88-1.15 deg** | 3.8-4.3 deg | 28-37 deg |
| RGE ALTI | **0.00 deg** | 0.00-0.01 deg | — |

Over Lac d'Annecy RGE ALTI is a single constant (446.88 m across the whole
tile); LIDAR HD carries 18-24 mm of ripple. ~1 deg of mean normal tilt on a
surface the eye expects mirror-flat, i.e. more than the entire 0.87 deg
quantisation budget, on the worst possible surface for it. Also checked at
Serre-Ponçon; same result.

**RGE ALTI over WMS carries no information below 4.77 m — STRUCK.** An earlier
version of this section proposed sourcing the 2.38 m ring from RGE ALTI over
WMS, on the strength of it being "genuinely finer than L14 (0.736 m RMS beyond a
bilinear upsample)". That was wrong. RGE ALTI's WMS output at 1024 px over an
L14 footprint is **nearest-neighbour replication of the 4.77 m grid**: 100% of
4x4 blocks are bit-identical, 16 distinct values along 64 samples, effective
spacing exactly 4.77 m. The 0.736 m "detail" was the gap between nearest and
bilinear upsampling — pure artefact. Its 4x4 box-mean matching L14 to RMS
0.0000 m is a consequence of replication, not of nesting.

The L14 ceiling is therefore a **real data limit** for RGE ALTI HIGHRES, and
blockiness is worse than smoothness for rendering: flat 4.77 m plateaus with
vertical steps give normals that are either 0 or 90 degrees.

How it was caught, for reuse (the tests are cheap, run them on any new source):

1. **Within-block spread** — decimate to the suspected source grid, check
   whether fine samples vary inside each coarse cell. Decisive here; run first.
2. **Autocorrelation vs lag 1..12** — real detail decays monotonically (LIDAR
   HD: 0.73 0.43 0.30 0.17 0.05 ...). RGE ALTI peaked at lags 4, 8, 12 with
   0.975, 0.936, 0.907 — period exactly one source cell.
3. **Radial power spectrum slope** — catches white sensor noise as a flat
   high-frequency floor. **Failed here**: both products gave beta ~2.9, because
   a periodic artefact makes spectral peaks, not a floor. Not sufficient alone.
4. **Excess vs slope / land cover** — correlation noise concentrates on smooth,
   low-texture ground. Ambiguous here: the excess grew with slope, which fits
   both real relief and resampling error.

**Chain to build:**

Both fine rings come from **WMS LIDAR HD**
(`IGNF_LIDAR-HD_MNT_ELEVATION.MIXED.WGS84G`), each one 1024^2, each one request:

| ring | source | bbox | spacing | cost |
| --- | --- | --- | --- | --- |
| 0 - 1.22 km | WMS LIDAR HD, 1024^2 | 2x2 L14 footprints | 2.38 m | 1 req, 4.0 MB, 5.33 MiB |
| 1.22 - 2.44 km | WMS LIDAR HD, 1024^2 | 4x4 L14 footprints | 4.77 m | 1 req, 4.0 MB, 5.33 MiB |
| 2.44 - 9.77 km | WMTS L13 (existing) | — | 9.54 m | 13.4 MB |

8.0 MB and 10.67 MiB added. Both bboxes are grid-aligned and corner-anchored so
the user sits near the centre (see the centring note above).

An intermediate draft put the 4.77 m ring on WMTS L14. That came from the struck
RGE ALTI revision, where the point was "one product end to end"; once the 2.38 m
ring went back to LIDAR HD the rationale inverted — WMTS there *creates* a
product seam at +-1.22 km instead of avoiding one. With both rings on LIDAR HD
the server's exact box decimation (512 px = 2x2 box mean of 1024 px, RMS
0.0001 m) makes the 2.38 <-> 4.77 handoff exact, and the only product seam is
LIDAR HD against L13 at +-2.44 km, where the 3.14 m worst case subtends 1.3 px
rather than 2.7 px and there is room for a wide fade.

Costs of WMS over WMTS for the 4.77 m ring, accepted: +0.6 MB (WMS does not
compress), and the URL is shared only among locations in the same corner
quadrant instead of per tile. Neither touches the offline story, since L13
remains the WMTS backbone and the fine rings are not prefetched anyway.

Note the **`MIXED` fallback risk only bites below 4.77 m**: if it returns RGE
ALTI outside LIDAR HD coverage, that is still genuine data at 4.77 m, which is
RGE ALTI's real resolution. Only the 2.38 m layer needs the blockiness check.

**Water needs no work — the shader already discards the DEM normal on water.**
`terrain.frag` (~line 579) does
`final_normal = normalize(mix(final_normal, waterNormal, waterMask))`, and past
the 5 km wave fade `mix(final_normal, vec3(0,0,1), waterMask)`. At
`waterMask == 1` the DEM-derived normal is replaced outright, so the ~1 deg of
LIDAR ripple measured above never reaches the screen. What remains is invisible:
the mesh geometry still carries the ripple (28 cm total spread at Annecy, 0.14
mrad across a 2 km lake, i.e. 0.14 px, and lakes are never on the skyline); AO
bakes from the base relief only and never sees the LIDAR grid; and
`applyWaterEffects` replaces the albedo regardless. Only the shoreline band
(0.01 < waterFactor < 0.99) mixes in terrain normal, where the ground is rough
anyway.

Residual dependency, pre-existing rather than introduced here: the override is
driven by `waterFactor` from the OSM water layer, so a lake missing from OSM or
a pre-CLC4 tile without the water layer (`is_clc4`) would show the ripple.

Drop the 1.19 m layer for now: 0.423 m RMS is sub-pixel beyond ~440 m, so it
only firms up the immediate foreground, at 4x the cost. Easy to add later.

### Open

**ANSWERED — it was neither signal nor noise but nearest-neighbour replication;
see the struck paragraph above. RGE ALTI cannot source anything below 4.77 m.**

**What `MIXED` returns outside LIDAR HD coverage — affects the 2.38 m ring
only.** If it falls back to RGE ALTI, a 2.38 m request over an unflown area
returns the blocky replication: 4.77 m plateaus with vertical steps, worse than
dropping the ring entirely there. The 4.77 m ring is unaffected, since RGE ALTI
is genuine at that spacing. Test on an unflown area before shipping; the
within-block-spread check is cheap enough to run on the fetched grid itself and
drop the 2.38 m ring when it trips.

**Client-side water flattening — NOT NEEDED, struck.** Earlier drafts of this
section worked out how to flatten LIDAR HD's water from the OSM water layer
(CPU-rasterise the triangles, then set `fade = 0` so the sample inherits the
hydro-corrected coarse value, rather than computing a per-lake level). None of
it is required: `terrain.frag` already replaces the DEM normal with the
procedural water normal, or with `(0,0,1)` past the wave fade, wherever
`waterMask` is 1 — see the paragraph above. The measured ripple is a normal-only
artefact and the normal is discarded before it is used.

## Nested near field — SHIPPED (2026-08-04, branch `hd`)

What was built, and where it differs from the two design sections above.

### The chain as shipped

| ring | source | spacing | extent | cost |
| --- | --- | --- | --- | --- |
| 0 (inner) | WMS LIDAR HD `IGNF_LIDAR-HD_MNT_ELEVATION.MIXED.WGS84G`, 1024^2 over 2x2 level-14 footprints | 2.38 m N-S | +-1.22 x 0.85 km | 2x2 GetMaps, 4.0 MB |
| 1 | WMS LIDAR HD, same layer, 1024^2 over 4x4 footprints | 4.77 m | +-2.44 x 1.70 km | 2x2 GetMaps, 4.0 MB |
| 2 | WMTS `ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES` level 13, 8x8 (unchanged) | 9.54 m | +-9.8 x 6.8 km | 64 tiles, 13.7 MB |
| base | Copernicus `.dem` | ~30 m | +-63 km | hosted |

Rings 0 and 1 are the same layer at half the resolution, so they are exactly
box-consistent and the inner fade hides no product difference. Only ring 1's fade
onto l13 crosses products, and at `fade_metres = 600` it has four times the inner
ring's annulus for the same ~3 m worst case. Eight WMS requests per load against
the 40/s ceiling.

**Three rings as of 2026-08-04.** The 4.77 m ring for the 1.22-2.44 km band was
added afterwards, and cost exactly what the design predicted: a `wms_layer`
value, one entry in the fetch list, `hd_slots` 2 -> 3, and a third branch in the
three shaders. See the ring table below.

The 4.77 m ring was also going to come from WMS to keep the chain on one product.
It does not: ring 1 is still the WMTS block, so the LIDAR HD / RGE ALTI product
seam sits at +-1.22 km rather than +-2.44 km. The fade handles it -- each ring is
blended onto the surface beneath it, so it equals that surface at its own edge --
and `fade_metres` is 300 for the inner ring against l13's 1500.

### Commits

- `6fd44fd` parameterise `Hd_dem` by layer (pure refactor, RMSE 0)
- `19440d9` let `blend` chain onto any grid, not just the base tile (RMSE 0)
- `16bba21` the ring itself, end to end: `Wms` layer kind, uniform arrays,
  `sampleTerrainHeight` shared by the terrain, shadow and GPX path programs
- `5cacf1c` lift the eye to clear local relief
- `e42149f` step the visibility ray by its clearance

### L14-4 (per-layer height scale) — measured, demoted to cleanup

The gate above predicted terracing from the global u16 encoding at the ring's
spacing, on the strength of `atan(14.5 cm / 1.66 m)` = 5.0 deg. **That figure was
a worst-case bound and about 4x pessimistic.** Quantising the real 2.38 m grids
and recomputing normals gives, on the slopes where terracing would show
(under 10 deg):

| view | encoding | quantum | normal error |
| --- | --- | --- | --- |
| Emparis | global (9500 m) | 14.50 cm | mean 1.09 deg, p99 2.36 deg |
| Emparis | per-layer (756 m) | 1.15 cm | mean 0.09 deg, p99 0.19 deg |
| glacier basin | global | 14.50 cm | mean 1.11 deg, p99 2.41 deg |
| glacier basin | per-layer (1463 m) | 2.23 cm | mean 0.17 deg, p99 0.37 deg |

The realistic error is ~1.1 deg rather than 5, because the relief bake uses
central differences spanning two samples and neighbouring rounding errors partly
cancel.

**And it does not reach the screen.** Emparis is statistically unchanged with the
ring on (4494 -> 4519 distinct luminance levels, flat-neighbour fraction
9.3% -> 9.1%); the basin shows more flat neighbours (10.8% -> 13.9%) but *more*
distinct levels, so that is flat ice pans the finer grid resolves, not steps.
Amplified crops of the Emparis plateau show no contour-following bands that the
ring-off capture does not also have. `perturbNormal`'s procedural noise at
500/70/10 m almost certainly dithers over a degree or two.

So per-layer scale/offset is still worth doing -- 12x better for zero memory or
bandwidth, and it would unblock any finer ring -- but as cleanup, not as a fix
for a visible defect.

### Two things the plan did not anticipate

- **The eye could end up underground.** The eye sits 2 m above a single bilinear
  sample, which is safe at l13's 9.5 m but not at 2.38 m: the glacier view
  rendered a wedge of slope interior across the lower frame. It also silently hid
  every POI, because `Visibility.test_precise` starts its finely-stepped phase at
  the source. Fixed in `5cacf1c` by taking the highest sample within 4 m --
  testing true distance, not a sample count, which had lifted the Mont Blanc
  summit by 11.3 m.
- **`Visibility`'s whole-pixel tail mattered more than expected.** Fixed in
  `e42149f`: the middle of the ray now steps by its clearance above the terrain,
  and each refinement carries the finest step worth taking over it. 348 more
  occlusions found at Mont Blanc for ~3% more time.

### Verification

Frozen-clock headless captures at the Mont Blanc vista, glacier and
grazing-forest views (see [[headless-verification-rig]]):

- With the WMS ring blocked at the network layer, RMSE exactly 0 against the
  pre-change build. The whole restructure -- uniform arrays, sampler chain, blend
  chaining, list-based publish, texture slot arrays, three programs rewired -- is
  byte-identical when the new ring is absent.
- With it enabled, `1/1 tiles` for the WMS ring and `64/64` for l13, both
  blended; change concentrated in the near field.
- **Verified on device and looks good** (2026-08-04), which is what the rig
  cannot speak to: swiftshader is not a real driver and desktop runs `mediump` at
  fp32, so the fp16 normal precision, the two extra vertex-stage sampler-array
  entries and the two extra varyings were all untested until then. It also
  confirms the terracing result on hardware where fp16 could have worsened it.

### Still open

- The 4.77 m ring for the 1.22-2.44 km band (above).
- **The WMS ring is all-or-nothing.** One request, so a single transient failure
  drops the whole ring -- hit twice during testing. It falls back to l13 cleanly,
  but 2x2 requests of 512^2 would be the same bytes with graceful degradation,
  reusing the partial-block path the WMTS path already has.
- ~~What `MIXED` returns outside LIDAR HD coverage~~ **CLOSED (2026-08-04).** The
  guard is not worth building. Coverage is essentially complete across the Alps,
  and probing the ring bbox at the other extremes of the data footprint returns
  genuine sub-5 m data everywhere tried -- Monte Cinto in Corsica, and Le Maïdo
  and Piton des Neiges on Réunion, i.e. the overseas département, which was the
  least likely to be covered:

  | site | detail beyond its own 2x decimation | autocorrelation, lags 1..8 |
  | --- | --- | --- |
  | Plateau d'Emparis | 0.630 m RMS | 0.58 0.23 0.13 0.08 ... |
  | Monte Cinto | 0.944 m | 0.56 0.21 0.11 0.08 ... |
  | Le Maïdo | 0.555 m | 0.47 0.09 0.00 0.01 ... |
  | Piton des Neiges | 1.115 m | 0.55 0.14 0.04 0.02 ... |

  Monotonic decay at every site and no nodata, against RGE ALTI's replication
  signature of 0.975 / 0.936 / 0.907 at lags 4, 8, 12. Note the distinction that
  makes this less alarming than it looked: that replication came from requesting
  the *RGE ALTI layer* below its native level, not from `MIXED`. Where `MIXED`
  lacks LIDAR it most likely returns nodata, which `blend` already fades into
  ring 1. Only 64/64 distinct values along a row would not have been enough to
  conclude this -- a smooth upsample passes that test, which is why the
  autocorrelation is the one to run.
- Per-layer height scale (above), as cleanup.

## Correction: the development HTTP 400s were self-inflicted (2026-08-04)

Commit `82a2203` justifies the 2 x 2 split partly by "the geoplateforme
intermittently answers HTTP 400 to valid WMS requests under concurrency". That
reading is wrong and the message should not be trusted on it.

The actual rule: **the WMTS is not rate limited at all**; the WMS endpoint is,
at 40 requests per second, and exceeding it returns 400 for everything for the
next five seconds. A location load spends 4 of that allowance -- the four pieces
of the inner ring -- and the 64 WMTS tiles and the 256-request prefetch spend
none. The app is nowhere near the ceiling.

The 400s seen while developing came from *test* traffic: this session fired many
curl bursts at the WMS endpoint (coverage probes, the piece-reassembly check,
three rounds of latency comparison), and with a five-second penalty window those
landed on browser runs seconds later. The one run in four that fetched 4/4 was
simply the one after a pause.

A paced request gate was written and then reverted: at 45/s it would have added
~1.4 s to every load by spacing 64 exempt tiles, to solve a problem the app does
not have.

The split itself stands, on the narrower ground it was designed for: it is
pixel-exact (verified, 0 of 1048576 samples differ against the single request),
costs the same bytes, allows a partial cache hit, and turns any single-request
failure into a faded quadrant rather than no ring. Just do not expect it to be
fixing a regular occurrence.

Also corrected here, since two rounds of it are recorded above: the tile losses
that looked like a service-worker defect were the test harness, not the app --
see [[headless-verification-rig]] for the two traps (single-threaded HTTP/1.0
server; Chrome's storage quota against a full /tmp).

## Behaviour at the border (checked 2026-08-04)

Several featured locations sit on or near the Italian frontier -- Baisse de
Druos is essentially on it, and Mont Ténibre, Col Girardin and the Aiguille du
Brévent are close. Fetching the 2.38 m ring there:

| site | out-of-coverage fill | share of the block | where |
| --- | --- | --- | --- |
| Baisse de Druos | -9999 | 9.9% | rows 0-539, cols 376-1023 (north-east) |
| Mont Ténibre | -9999 | 8.7% | rows 0-565, cols 427-1023 |

-9999 is well below `nodata_limit` (-500), so `blend` treats it as nodata,
builds the distance field and fades to the ring beneath over `fade_metres`
(300 m for the inner ring). Fading a 3 m product difference across 300 m is a 1%
slope change. No `0`-fill plateau lurks among the valid samples -- their minimum
is 2279 m and 2496 m respectively, and no value repeats more than ~50 times in
950k.

Two nested fades therefore run along the frontier: 2.38 m into l13 over 300 m,
and l13 into the Copernicus base over 1500 m (RGE ALTI tiles east of the border
404, which has always been handled).

The eye is safe at such a viewpoint: it reads the *blended* grid, which carries
the coarser surface's height throughout the nodata region, so there is no
underground case even standing on the frontier.

Corrected while checking: the `nodata_limit` comment attributed -99999 to IGN.
That is this module's own pre-fill for a request that never arrives; IGN's
out-of-coverage value is -9999. Both are below the limit, so nothing was broken,
but anyone tightening it on the strength of that comment would have been.

## Per-grid height quantisation — DONE (2026-08-04)

The last item from L14-4. Every blended grid now carries its own `height_scale` /
`height_offset`, spanning only the height range it holds instead of the .dem
pipeline's 9500 m, and the relief pyramid is baked and decoded through the same
pair. Measured at the Mont Blanc vista:

| grid | height range | step | was |
| --- | --- | --- | --- |
| l13, +-9.8 km | 3972 m | 6.06 cm | 14.50 cm |
| ring 1, +-2.44 km | 1960 m | 2.99 cm | 14.50 cm |
| ring 0, +-1.22 km | 1080 m | 1.65 cm | 14.50 cm |

So 2.4x to 8.8x finer, including l13, which sat exactly at the 0.87 deg
acceptance bar and is now well inside it.

**The stated justification for doing it was wrong, and is worth recording.** Ring
1 was described as "~2.5 deg of normal error", from `atan(14.5 cm / 3.31 m)` --
the worst-case single-step bound, which the section above had *already*
established is about 4x pessimistic against a measured mean. Ring 1's realistic
figure was ~0.6 deg, i.e. already inside the bar. This was cleanup throughout,
as the earlier measurements said, not a fix for anything over the line.

Scope, for anyone touching the height encoding: `Hd_dem.source` and `Hd_dem.t`
carry the pair; `blend` computes in metres into a float32 scratch (4 MB at 1024
samples, 16 MB at 2048) and quantises in a second pass once the range is known;
`Hd_dem.get_height` must be used for a ring grid rather than
`Dem_loader.get_height`, which assumes the base scale; `rendered_height` takes an
accessor rather than a grid; `compute_relief` takes the pair and uploads it to
normal.frag and downsample.frag; `sampleReliefHeight` takes it as parameters, the
base path passing constants; and `hd_params` carries it per ring. `Dem_loader` is
untouched, so the base path is bit-identical by construction.

Verified: renders indistinguishable from the global-scale build (RMSE 0.2-5.9%,
consistent with sub-decimetre height changes and no artefacts), all three rings
loading, blend cost unchanged.

## Blend cost: 2x, from getting Bigarray out of the hot paths (2026-08-04)

Phase timing of `Hd_dem.blend`, measured in the browser:

| grid | scan | nodata_distance | resample | quantise | total |
| --- | --- | --- | --- | --- | --- |
| l13 2048^2, before | 53 | **824** | 349 | 226 | 1452 ms |
| l13 2048^2, after | 87 | 180 | 448 | 0 | **714 ms** |
| ring 1 1024^2 | 5 | 16 | 40 | 0 | 65 ms |
| ring 0 1024^2 | 9 | 0 | 42 | 0 | 51 ms |

The distance transform was 57% of the l13 blend, not the resampling. It runs at
Mont Blanc because the block straddles the Italian border, so part of it is
nodata.

**Bigarray element access is not compiled to a plain load by wasm_of_ocaml
today** (confirmed by the author). That is the whole story:

- The transform's scratch field was a Bigarray, touched ~50 million times across
  the two sweeps. Switching it to `Bytes`: **824 -> 115 ms**. De-closuring the
  inner loop first made no difference at all (824 -> 896), so the per-sample
  `ref` and `consider` were already being optimised away -- it was the array.
- The float32 scratch holding every blended sample, plus the second pass to
  quantise it, cost 226 ms and 16 MB. Both are gone: every blended value is
  `b + f * (h - b)` with `f` in [0, 1], so it lies on the segment between the two
  surfaces and the range is *bounded* by the union of theirs. Scanning the source
  window (~635^2 for l13) gives that bound cheaply, and the loop quantises inline.
  The bound is wider than the truth, so the step is coarser: 7.98 cm against
  6.06 cm, still well inside the 14.5 cm the base scale imposed.

Note for the original question, which was whether a web worker and a piece of
wasm would help as they do for the `.dem` tiles: the viewer already ships as
wasm (`viewer.bc.wasm.js`, `wasm_of_ocaml`), so there is no "move it into wasm"
win to be had -- blend was always wasm. The existing worker is JS (`(modes js)`)
and gets its speed from hand-written SIMD `.wat`, so moving this float-heavy loop
into it as-is could be slower.

Still available, in order of value:

- **Offload to a worker for latency, not throughput.** ~830 ms still runs on the
  main thread before the location can be published. Each blend reads only a
  *window* of its source -- ~635^2 samples (0.8 MB) for l13 over the base, ~512^2
  for the rings over their predecessor -- so the transfer is small; there is no
  need to ship the 32 MB base tile. The worker would have to be built to wasm
  (`(modes js wasm)`) or the loop written as `.wat`, or it will lose more than it
  gains.
- **The remaining resample cost is Bigarray too**: 4M reads of `src` (float32)
  and 8M writes of `out` (int8). Removing those needs a bulk Bytes-to-Bigarray
  path or hand-written wat with SIMD.

## `blend.wat` — SHIPPED in the viewer, 3x (2026-08-04)

Landed as `acbb8c2`, but **in the viewer, synchronously**, not in the worker as
the design below assumed. The linear-memory win was the separable half and it is
the larger one; the worker move is still open (see the end of this section).

Two commits:

- `bfd1fa6` extracts the arithmetic into `Blend_core` — plain arrays and scalars,
  no Brr, no Lwt — which is the boundary the wat has to reproduce. Verified RMSE
  exactly 0 on all three pinned views.
- `acbb8c2` adds `blend.wat` plus `Blend_wasm`, which drives it exactly as
  `Worker.Wasm` drives the other two modules.

Mont Blanc, medians of three warm reps:

| ring | OCaml | wasm | |
| --- | --- | --- | --- |
| l13 2048^2 | 240 ms | **79 ms** | 3.0x |
| 4.77 m 1024^2 | 63 ms | **24 ms** | 2.6x |
| 2.38 m 1024^2 | 44 ms | **13 ms** | 3.4x |
| total | 347 ms | **116 ms** | 3.0x |

And that is *with* 16 MB of samples copied into linear memory and 8 MB of output
copied back on every call — see "still available" below.

### How it was verified

The rule written into the design below — do not trust a reference you wrote
yourself — was kept, but by a cheaper route than a Node harness: run **both
implementations in the browser on real blocks** and diff the output. Byte
identical on all nine blends of the three views, with identical `height_scale`
and `height_offset`, then RMSE exactly 0 with the wasm path alone.

That also settled a question the timing numbers could not: the nodata path is
genuinely exercised, not merely assumed. Mont Blanc's l13 block holds 355 nodata
samples and its 4.77 m ring 337105 — a third of the block — so both sweeps of the
distance transform ran under the diff.

`Blend_core` stays: it is the reference for any future change to the wat, and the
fallback if the module has not finished instantiating (`Blend_wasm.run` falls back
silently, so callers need no branch).

### Still available, in order of value

- ~~**The 24 MB of copying per call.**~~ **Measured, and not worth it** (see
  "The copying is 13 ms" below).
- **The worker**, for latency rather than throughput: 116 ms still runs on the
  main thread before the location can be published. Now much less urgent than
  when it was 830 ms, and still blocked on the same thing — `chain` is called
  from a synchronous block that deletes the previous location's textures and
  publishes with no `draw` in between, so making `blend` async is a real
  restructuring, not plumbing.
- **SIMD.** The resample is f64 throughout and could be f64x2, but the loop is
  gather-heavy (`rowv[bx[j]]`), so this is speculative.

### The original design, kept for the record

### Why the worker and not the viewer

`grep -c WebAssembly src/web/viewer.ml` is **0** -- the viewer instantiates no
wasm module of its own, it *is* one. `worker.ml` already owns the whole pattern:

- a single `WebAssembly.Memory` (`initial 300`, `maximum 4096` pages),
- `Wasm.get_ba ()`, a Bigarray view over `memory.buffer`, used to place inputs at
  explicit integer pointers,
- exported functions called with pointers plus dimensions (`decompress_simd`),
- results bulk-copied out with `Bigarray.Array1.blit` into a fresh `Brr.Tarray`
  and transferred.

Adding wasm instantiation to `viewer.ml` instead would get the linear-memory win
but leave the work on the main thread, which is the larger of the two prizes:
~830 ms currently runs before the location can be published.

### What the module has to carry

The worker is `(modes js)`, so the wat must contain the entire hot path, not just
a kernel. All three phases of `blend`:

1. the nodata scan over the raw samples,
2. the two-sweep chamfer distance transform (already `Bytes`, so this is a
   straight port -- and the 824 -> 115 ms measurement above is exactly the win
   this phase already banked, i.e. do not expect a second one here),
3. resample + fade + inline quantise, which is where the remaining ~450 ms sits.

### The data actually moves cheaply

The reason this is worth doing at all: **each blend reads only a window of its
source.** ~635^2 samples for l13 over the base, ~512^2 for the rings over their
predecessor. The 32 MB base tile never needs transferring.

Both sides are already byte arrays, which makes the marshalling nearly free:

- `Dem_loader.t.data` is `int8_unsigned Array2`, rows of `2 * size` bytes,
  little-endian u16 pairs (`get_height` reads `col*2`, `col*2+1`). Extracting the
  window is one row-wise blit per row; the wat reads u16 with `i32.load16_u`.
- the output is the same layout, so what the wat writes copies straight into the
  texture-upload array with no repacking.
- only `raw.samples` is float32 (`layer.size^2`), and it comes from the fetch
  path, so it can be written into linear memory directly rather than copied.

### Pieces to build

- `src/web/blend.wat`, `(import "env" "memory" (memory 1))` like the other two.
  For scale: `decompress_tile.wat` is 181 lines, `decode_clc.wat` 342.
- a dune rule mirroring the existing ones:
  `(run wasm-opt -O4 --enable-simd --enable-bulk-memory %{deps} -o %{target})`.
- a `Blend` request variant in `Worker_pool` / `worker.ml` alongside `Decode`.
- `Hd_dem.blend` becomes async: extract the source window (~8 ms of Bigarray
  reads on the main thread, unavoidable), post, await. `chain` and `publish`
  already tolerate a late ring, so the call-site change is small.

Rough size: ~300-400 lines of wat, ~100 of plumbing.

### Expected prize

Extrapolating from the one clean measurement of the same swap (the distance
transform's scratch, 7x on an unchanged algorithm): l13 blend ~714 -> ~250 ms,
and ~830 ms of total blend time leaves the main thread. Latency, not throughput.

### Verify it offline first

This would be the least verifiable code in the repo -- a pointer slip yields
silently wrong terrain, not a crash, and this branch has already produced several
measurement-driven reversals. So: drive `blend.wat` from **Node** against a real
fetched tile and diff its u16 output byte-for-byte against a reference model of
the current blend, before any of it touches the app. Only then wire up the
worker, and gate that step on the usual RMSE-0 capture.

*(What was actually done: the same rule, but the diff ran in the browser against
`Blend_core` on real blocks, which needed no harness at all and covered the
nodata path that synthetic Node inputs would have had to fake.)*

## The copying in and out of linear memory is 13 ms — measured (2026-08-04)

Correction to the "still available" list above, which called the 24 MB of copying
per blend the highest-value item left. It is not. Phase breakdown at Mont Blanc,
`in` being the samples copied into linear memory and `out` the result copied back:

| ring | in | win | wasm | out | total |
| --- | --- | --- | --- | --- | --- |
| l13 2048^2 | 6.7 | 0.3 | **66.0** | 3.7 | 78 ms |
| 4.77 m 1024^2 | 0.4 | 0.1 | **15.3** | 1.1 | 18 ms |
| 2.38 m 1024^2 | 0.3 | 0.1 | **9.4** | 0.8 | 11 ms |

All the copying together is **13.5 ms of 107**. 6.7 ms for 16 MB is about
2.4 GB/s, i.e. `Bigarray.Array1.blit` already compiles to a memcpy -- unlike
element access, which is the whole reason this module exists. The wat itself is
85% of the cost.

### What removing it would actually take

Both halves need the same enabling change: **the memory may never grow again**,
because `WebAssembly.Memory.grow` detaches the buffer and every view over it.
That means reserving the static worst case at startup instead of growing on
demand.

- **Samples in (7.4 ms).** `Hd_dem.fetch` writes tiles into the block as they
  arrive, so the block would have to be a region of linear memory. Three layers
  are fetched concurrently, and a stale fetch from a previous location can still
  be in flight, so the regions cannot be per-layer -- they need an allocator with
  a free list. Worse, a raw block is blended *twice* (once from `settled` for the
  early publish, once from `Lwt.all` for the refinement), so a slot cannot be
  freed after its blend; its lifetime is the location epoch. Reserve 2 epochs
  worth: ~48 MB.
- **Result out (5.6 ms).** The blended grid would be a view into linear memory
  rather than an independent bigarray. That needs no change at the consumers
  (a view *is* a bigarray, so the bake, `get_height` and the next ring's
  `source_window` all keep working) but it introduces aliasing: a published grid
  would sit in a region the next location's blend can reuse, and getting that
  wrong renders subtly wrong terrain rather than failing. Needs double buffering,
  so ~24 MB of outputs rather than 12.

So: ~50 MB permanently reserved (against ~29 MB that the memory grows to today
and never releases), a new allocator coupled to `Hd_dem`'s epoch logic, and one
new class of silent-corruption bug -- to save 13 ms of a 107 ms path, on an
Android-first target. Not done.

If the blend needs to be faster, the 66 ms is the target, not the 13.

## Attacking the wat's own 66 ms: the distance transform (2026-08-04)

In-wat phase profile at Mont Blanc, minima of several runs, via a temporary
phase-limit parameter on the export:

| phase | l13 2048^2 | 4.77 m (337k nodata) | 2.38 m (no nodata) |
| --- | --- | --- | --- |
| scan | 11.0 | 1.4 | 1.6 |
| **nodata_distance** | **53.7** | 7.9 | 0 |
| precompute | ~0 | ~0 | ~0 |
| range scan | 4.7 | 1.4 | 1.5 |
| resample+quantise | 45.8 | 13.9 | 13.5 |

(Absolute numbers are inflated -- the profiling build calls the export 15 times
per blend -- but the split is stable and reproduced across two rounds.)

The distance transform was the largest single phase, and it was doing bounded
work globally: Mont Blanc's l13 block has **355** nodata samples out of 4.2M,
0.008% of it, and the transform swept all 4.2M cells twice for them.

### The fix: bound it to the nodata box

The scan already visits every sample, so it now also tracks the bounding box of
the nodata samples (on the rare branch, so it costs nothing measurable). The
transform runs over that box grown by `ceil(fade_nodata)`, with the rest of the
raster filled with 255.

This is **exact, not approximate**:

- a cell whose true distance is below `fade_nodata` is within that many texels of
  some nodata sample, so it is inside the grown box; and the propagation path
  from that sample to it is monotone in both axes, so it lies inside the box too
  and the sweeps reach the same value;
- every cell outside has a true distance above `fade_nodata`, where only "at
  least `fade_nodata`" is observable, because `smoothstep` saturates at 1;
- treating the box edge like the raster edge is exact for the same reason the
  original edge guards are: the skipped neighbour would have read 255, which is
  what `m` starts at in the forward sweep and above every `m` in the backward one.

The one precondition is `ceil(fade_nodata) <= 255`, or the distances outside the
box would be observable through the saturation; above that it falls back to the
whole raster. No layer is close today -- l13 is 157, both rings 126.

### Result

| | before | after |
| --- | --- | --- |
| l13, 355 nodata samples | 53.7 ms | **1.2 ms** |
| 4.77 m, 337105 nodata samples | 7.9 ms | **3.8 ms** |
| full l13 call, same conditions | 114.1 ms | **52.8 ms** |

Byte-identical to `Blend_core` on all nine blends of the three views, and RMSE
exactly 0 against the previous `blend.wasm`. Note the second row: the win is not
merely "nodata is rare" -- even a block that is a third nodata halves, because
the nodata is still confined to part of it.

### What is left in the wat

`resample+quantise` is now the dominant phase, then `scan`:

- **The scan** is 4M f32 loads with three compares, a clean f32x4 candidate
  (nodata lanes replaced by +/-inf, then a horizontal reduce; min/max are exact
  and order-independent, so it stays bit-identical). The nodata bounding box can
  be tracked per 4-lane group rather than per sample -- a box that merely
  *contains* the nodata box is still exact, just marginally larger.
- **The resample's interior fast path: tried, byte-identical, and not faster.**
  See below.

## Rejected: the resample's interior fast path (2026-08-04)

Wherever `t >= 1` -- about 66% of an l13 block and 49% of each ring, the interior
inside the fade annulus -- `smoothstep` is exactly 1, so the blended value is
`b + 1.0 * (h - b)`, i.e. just the refinement. Skipping the horizontal
interpolation and the smoothstep there looked like the obvious next win.

**It is byte-identical.** The worry was that `b + (h - b)` equals `h` only up to
rounding; in practice Sterbenz's lemma applies (any two terrain heights in one
block are within a factor of two of each other), and the diff confirmed it: zero
differing bytes across all nine blends of the three views, 12.6 MB of output.

**But it is not faster.** Within-process minima of ten calls on identical data,
only `blend.wasm` swapped between two servers:

| ring | box only | + interior fast path |
| --- | --- | --- |
| l13 2048^2 | 51.3 | 50.4 |
| 4.77 m 1024^2 | **17.7** | 21.7 |
| 2.38 m 1024^2 | **12.6** | 15.7 |

Neutral on l13, about 25% worse on the two 1024 rings, reproduced by a
page-load-level A/B as well. The lesson is where the cost actually sits: the
interpolation it skips is four loads and three flops from a 5 kB `rowv` that is
L1-resident throughout, which is nearly free, while the per-sample branch it adds
divides the loop body into two divergent paths and costs more than that. Reverted.

**Hoisting the branch out of the inner loop does work**, and by more than
predicted -- see the next section. The interior samples were never the problem;
deciding per sample that they were interior was.

## Column spans instead of a per-sample test: -31% (2026-08-04)

The rejected version above asked, for every sample, "is the refinement fully
faded in here?". This one answers that per *row*, once, and then runs two
branch-free loops.

`edge_x` rises and then falls, so the columns where the edge fade is complete
form one contiguous range `[flo, fhi]`, computed once per call. `edge_y >= 1` is
a per-row scalar. And when the block holds nodata, a row outside the rows of the
box the distance transform wrote has distance 255 at every sample -- hence a
complete nodata fade -- and contains no nodata sample itself, both because every
nodata sample is inside that box. So a row meeting all three conditions splits
into `slow(0, flo-1)`, `fast(flo, fhi)`, `slow(fhi+1, last)`.

The fast span is three instructions of real work per sample: load the f32,
quantise, store. No interpolation of the surface beneath, no `edge_x` load, no
`min`, no nodata test, no `smoothstep`. The per-row state (`src_row`, `dst`,
`edge_y`) moved into globals so the two span functions can share it.

Within-process minima of ten calls on identical data, three interleaved rounds,
only `blend.wasm` swapped:

| ring | box only | spans | |
| --- | --- | --- | --- |
| l13 2048^2 | 54.6 | **36.8** | -33% |
| 4.77 m 1024^2 | 22.1 | **16.2** | -27% |
| 2.38 m 1024^2 | 15.8 | **10.5** | -34% |
| total | 92.5 | **63.5** | -31% |

Byte-identical to `Blend_core` on all nine blends of the three views, and RMSE
exactly 0 against the previous `blend.wasm`.

So the interior samples were never the problem -- deciding per sample that they
were interior was. Same arithmetic, same skipped work, 30% instead of -25%.

### Where the blend stands now

The l13 blend over the base tile, at Mont Blanc, across this session:

| | ms |
| --- | --- |
| OCaml, Bigarray scratch (before 2026-08-04) | 1452 |
| OCaml, `Bytes` scratch + bounded output range | 714 |
| wat on linear memory | ~240 |
| + distance transform bounded to the nodata box | ~54 |
| + column spans | **~37** |

Remaining, in order: the `scan` phase (~11 ms of the 37, a clean f32x4 candidate
that stays bit-identical), then the `slow` spans and the range scan. The 13 ms of
copying in and out is now a third of the total rather than a tenth, but the
lifetime and memory costs of removing it have not changed.

## Rejected: an f32x4 scan (2026-08-04)

The phase profile put `scan` at 11 ms of the l13 blend's 37, and it is the most
vectorisable loop in the file: 4M f32 loads with three comparisons. Implemented
as `f32x4`, with nodata lanes replaced by +/-inf so they cannot win, and `pmin`
and `pmax` -- whose "return the accumulator if either side is NaN" behaviour is
exactly what `if x < lo then lo := x` does, so the accumulators can never become
NaN and the vector and scalar versions agree even on input the service should
never produce. The nodata bounding box was tracked per four-lane group rather
than per sample, which is sound because the box only has to *contain* the nodata.

Byte-identical, `height_scale` and `height_offset` included. But the measured
gain, in the two rounds where the machine was quiet enough for the paired
measurements to be comparable, was **2-3% of the l13 blend** -- against a
predicted 15-20%. Six interleaved rounds never converged: the best `span` figure
was 48.6 ms and the best `simd` figure 34.4, but the round that produced 34.4
measured 90.0 for `span`, and the cleanest paired round gave 48.8 against 49.8.

Reverted. Not because it is wrong -- it is verified -- but because a gain that
cannot be demonstrated above the noise floor does not pay for eighty lines of
hand-written SIMD, plus a scalar fallback path, in the least reviewable file in
the repository.

The likely reason the phase profile overstated the opportunity: the scan streams
all 16 MB of samples, so it is bounded by getting them into the core rather than
by the arithmetic done on each one, and issuing four comparisons at a time does
not make the bytes arrive faster. If that is right, no amount of work on this
loop pays, and the same caution applies to the range scan.

What that leaves, if the blend needs to be faster again: the `slow` spans (still
the largest phase), or moving the whole thing off the main thread -- which buys
latency rather than throughput, and is still blocked on `chain` being called from
a synchronous publish block.

## Visibility: where its 800 ms goes (2026-08-04)

Now the largest per-location cost -- bigger than the blend, and it runs once per
publish, so up to three times since rings publish independently. Measured at Mont
Blanc with step counters in the march:

| | |
| --- | --- |
| rays (POIs within 70 km) | 4813 |
| near-phase steps (fixed 0.02 px over the first 6 px) | 877771 |
| adaptive-phase steps | 950598 |
| handovers to the Bresenham walk | 2689 |
| total | ~1.83M steps in ~800 ms |

So **~440 ns per step**, and the near phase -- 182 steps per ray on average, out
of a fixed 300 -- is 48% of them.

### Rejected: removing the per-step allocations

Each step allocated a `float option` per refinement consulted (`sample`) plus a
tuple (`terrain_at` returning height and step floor). Two million steps, so
several million allocations. Replacing the option with a `nan` sentinel and the
tuple with an out-parameter is **behaviour-identical** -- step counts matched to
the digit, 877771/950598/2689, and the same 1608 POIs kept -- but **not faster**.
Five paired rounds, only the two builds swapped, gave ratios 0.92, 0.54, 0.99,
1.39, 0.88: no effect above a noise floor of +/-2x. Reverted.

Two things worth keeping from the attempt:

- **`float ref` boxes.** The first version held the step floor in a `float ref`,
  which was *reliably slower* than the tuple it replaced (ratios 1.21, 1.16,
  1.27). `ref` is polymorphic, so its field cannot be unboxed and every
  assignment allocates a boxed float. A one-field `{ mutable f : float }` record
  is flat and does not.
- **`Float.is_nan` is a call.** Inlining it as `h <> h` moved the ratios from
  mostly above 1 to mostly below it -- suggestive, though still inside the noise.

### What to try instead: bound the near phase instead of walking it

The near phase walks 6 px at 0.02 px for *every* ray, unconditionally, because
`max_slope` is not a valid bound a few metres from the eye. But whether it can
reject at all is decidable in advance. Rejection at parameter `t` means
`terrain(t) - src_h - 0.1 > rise * t`, so with

    bound = max over the near disc of (terrain(p) - src_h - 0.1) / t(p)

any ray with `rise >= bound` provably cannot be rejected in the near phase and
can jump straight to `t = near_distance`. Computing it over grid *cells* rather
than along rays makes it conservative -- a bilinear sample never exceeds its
cell's corners -- so verdicts cannot change, only the step count falls.

One sweep of the cells within 6 px of the observer, over each grid: ~32k cells
per location, once. On a summit the bound is roughly the local slope angle, so
rays to anything on the horizon skip the phase entirely; rays into a nearby wall
do not, and would have rejected in a few steps anyway. Per-sector bounds (say 64
azimuth sectors) would recover the cases where one steep neighbour raises the
global bound for every direction.

The same reasoning applies to the adaptive phase, whose floor step inside the l13
ring is 0.1545 px (4.8 m), so a grazing ray can take ~2000 steps there; the lever
there is a max-height pyramid to skip open air, which is a bigger change.

Also cheap and not yet done: `near_step` is 0.02 px, but the finest ring's own
`step` floor is 0.0386 px (half of 2.38 m). By the argument already used for
`base_step` -- sampling a bilinear surface finer than half a cell adds nothing --
the near phase is walked about twice as finely as any data justifies.

### Done: the near phase's step comes from the finest grid (2026-08-05)

`near_step` was the constant 0.02 base pixels (0.6 m). The finest ring's own
`step` floor is 0.0386 (half of 2.38 m), and sampling a bilinear surface finer
than half a cell tells us nothing new -- the argument `base_step` already rests
on. Taking it from `fine` instead of a constant:

| | before | after |
| --- | --- | --- |
| near-phase steps | 877771 | **456307** |
| adaptive-phase steps | 950598 | 950567 |
| total | 1.83M | **1.41M** |
| pass | 901 ms | **594 ms** |

Not quite verdict-identical: POIs kept go 1608/1128/83 to 1608/**1129**/83 over
the three pinned views, so one marginal POI of 2819 becomes visible at the
glacier. All three views are RMSE exactly 0 regardless -- that POI is kept but
not drawn. For scale, the same measurement that justified this phase found that
*removing* it let 125 POIs through; halving its resolution lets one.

The other two attempts of this session on visibility are recorded above as
rejected: removing per-step allocations (no effect), and the near-disc bound.

### Why the near-disc bound does not work, in detail

Worth writing down, because the idea is sound and the reason it fails is not
obvious. `bound = max (terrain - src_h - 0.1) / t` is dominated by terrain *close*
to the eye, since `t` is the divisor. A single global bound is therefore set by
whatever is steepest anywhere in the disc and is vacuous: measured, it produced
zero skips.

Per azimuth sector does not rescue it. Cells close to the eye subtend wide
azimuth ranges, so a conservative implementation must mark each of them across
many sectors -- and the cell the observer stands in spans all of them. Excluding
an inner disc (`near_inner`) removes that cell but not the problem: the cells
just outside it still span tens of degrees each, and at Mont Blanc there is
terrain above eye level within 180 m, which then poisons most sectors. Measured
with 64 sectors and a 1-pixel inner disc: still zero skips.

What could work is an actual horizon profile -- march one ray per azimuth for N
azimuths and take, for each sector, the max over it and its neighbours -- which
is O(N x 300) instead of O(rays x 300) and beats the current cost for N well
below 4813. That is an approximation needing a safety margin, not a conservative
bound, so it would have to be justified by measurement rather than by argument.

## 1.19 m from the LIDAR HD WMS: the data is genuine (measured 2026-08-05)

One footprint-aligned bbox at Mont Blanc (45.833, 6.865; steep, 3861-4652 m over
1.2 km), fetched at 256/512/1024 px = 4.77/2.38/1.19 m.

- **Not replicated.** 0 of 262144 2x2 blocks of the 1.19 m grid are constant, and
  row-to-row differences are the same on even and odd rows (0.6678 / 0.6664 m):
  no nearest-neighbour signature. RGE ALTI below level 14 was 100% replicated, so
  this had to be checked rather than assumed.
- **Exactly box-consistent.** The 2.38 m output is the 2x2 box mean of the 1.19 m
  output to float32 rounding: RMS 0.000185 m, max 0.000488 m. Same property the
  512/1024 pair has, so the two nest with no product discrepancy and a fade
  between them hides nothing.
- **The step is worth about 55% of the previous one**, and that ratio is stable:
  at this footprint 4.77 -> 2.38 m is RMS 1.304 m and 2.38 -> 1.19 m is 0.711 m
  (ratio 0.55); the four-location averages were 0.693 and 0.423 (ratio 0.61).
  Absolute detail is roughly twice the four-location average on terrain this
  steep.

### Recommendation: upgrade the inner ring, do not add a fourth

`lidar_2m` is `steps:2, size:1024`. Making it `steps:2, size:2048` gives 1.19 m
over the *same* +-1.22 km extent, and costs nothing structurally: `HD_SLOTS`
stays 3, no new vertex-stage sampler, no new varying, no texture unit past 13, no
extra publish -- and therefore no extra relief bake, normal pass or **594 ms
visibility pass**, which is now the dominant marginal cost of a ring.

Price: 4.0 -> 16.0 MB on the wire (4 GetMaps instead of 1, the endpoint does not
compress) and 5.33 -> 21.33 MiB of GPU. The latency that made 16 MB unattractive
in the earlier analysis is what 9d09924 fixed: the inner ring can take its ~4 s
and publish when ready without holding the others.

Quantisation is fine at that spacing thanks to per-grid scales: ~1.23 cm steps
over 1.19 m is a 0.59 degree normal error, inside the 0.87 degree bar L13 shipped
with. It would not have worked before per-grid quantisation.

Still to check before committing the bytes: LIDAR HD MNT coverage at the featured
locations (gaps already cost tiles at the forest viewpoint), and that four
concurrent 1024^2 GetMaps do not trip the 40 requests/second WMS limit alongside
the other rings.

### Reverted: 1.19 m is not visible (2026-08-05)

Jérôme could hardly tell it from 2.38 m on device, and the reason is that the
0.711 m RMS above measures the *data*, not the screen. The 2.38 m grid is already
pixel-matched to 2.46 km while covering +-1.22 km -- finer than a screen pixel
everywhere inside its own extent -- so 1.19 m sits below the display's Nyquist
limit except with the camera almost on the ground, where it also has to compete
with its own 0.83 degree quantisation error and with the procedural bump and
triplanar detail. The four-location analysis that chose 2.38 m had it right; a
terrain-space residual cannot see a screen-space limit.

**Superseded the same day** -- see "1.19 m is masked, not absent" below. The
detail is there; the textures hide it.

## 1.19 m is masked, not absent (2026-08-05)

Jérôme rendered the same ground with the textures removed: the 1.19 m grid
carries **a lot** more visible detail than 2.38 m. So the revert above drew the
right observation and the wrong conclusion. What hides the extra resolution is
the procedural detail in `terrain.frag` -- the same machinery whose aliasing
`ac8ddc5` fixed -- not any limit of the display, and not the data.

That also explains the earlier confusion in both directions: my screen-space
argument for the revert was simply wrong (inside its own extent the ring is
*coarser* than a pixel, not finer -- see the footprint table in the
grazing-angle investigation), and the reason the difference was invisible was
never geometric.

**Method worth reusing:** to judge whether DEM resolution is worth anything at a
given range, render with the procedural detail off. With it on, the shading is
dominated by noise and the comparison is meaningless.

### The quantisation artefacts are the bounded range, not the sample count

Visible at 44.33652,6.91391 alpha=10 beta=82 zoom=0.50. `Blend_core` bounds the
output range instead of measuring it, deliberately, to avoid a second pass: it
takes the union of the refinement's range with the **source window's**. For the
innermost ring that bound is much looser than the truth, because the window spans
the whole +-1.22 km extent and so a large height range, while the ring's own
terrain may span little. Measured at Mont Blanc the bound cost 7.98 cm steps
against the 6.06 cm an exact range gives -- a third of the quantum wasted.

At 1.19 m spacing that waste lands on the normals: 1.72 cm over 1.19 m is a
**0.83 degree** normal error against the 0.87 degree bar L13 shipped with, i.e.
no headroom left. Which is where the artefacts come from, and they are visible at
2.38 m today.

So the fix looked like a tighter range: measure the true range, or bound it
against only the part of the source window the ring actually covers. **Measured,
and wrong -- see the next section.**

### The range bound is already exact to 1%, and the 7.98 cm figure was not slack

`Blend_core.measure` (temporary, reverted) recomputed each blend without storing
it, reporting every candidate bound side by side. At 44.73339,6.36308:

| ring | h range | source window | source under the fade only | exact output | step now | step exact |
| --- | --- | --- | --- | --- | --- | --- |
| l13 2048 | 2264.6 m | 2186.7 | 2186.7 | 2251.9 | 3.46 cm | 3.44 cm |
| 4.77 m 1024 | 1142.0 | 1134.1 | 1134.1 | 1128.3 | 1.75 | 1.72 |
| 1.19 m 2048 | 731.7 | 730.3 | 730.3 | 729.9 | 1.12 | 1.11 |

The bound is within **1%** of the exact range on all three. It could not be
otherwise: the refinement and the surface beneath are the same terrain, so their
ranges nearly coincide, and the union of two nearly equal intervals is not much
wider than either. Restricting the source scan to the fade frame -- the change
the section above proposed -- gains nothing at all: `b_frame` equals `b_all` to
the metre, because the extreme source values sit near the edges anyway.

The 7.98-vs-6.06 cm comparison that motivated this was not measuring slack. Those
are two different Mont Blanc runs, and the difference is the nodata region on the
Italian side: there the output *is* the base, so those base heights belong in the
range and an exact pass would keep them. There is nothing to buy here. Anyone
tempted again should re-run the measurement rather than trust either figure.

### The corrugation on smooth slopes is IGN's data, not the pipeline

Reported at 44.73339,6.36308 alpha=34 beta=77 zoom=0.88, with the procedural
detail off: a fine diagonal corduroy across every smooth slope, uniform pitch,
crossing terrain features rather than following them. Three things ruled out by
measurement, in order:

- **Not the height quantisation.** The blend's own step is 1.12 cm there, and it
  enters the Sobel as independent rounding: `q/6.64` = 1.7 mm of slope noise, or
  0.10 deg. Noise, and an order of magnitude below what is visible.
- **Not the RG8 normal encoding**, which was the best remaining suspect: the
  relief pyramid stores `n.xy * 0.5 + 0.5` in two bytes, so one LSB is
  2/255 = **0.449 deg** of tilt, a systematic staircase rather than noise, and
  half the 0.87 deg bar. Rebuilt the normal pyramid as RG16F holding raw `n.xy`
  (8x finer near flat, and `EXT_color_buffer_float` for renderability): the
  render changed by RMSE **0.15%** and the corduroy was pixel-for-pixel intact.
  Recorded because the reasoning was sound and the answer was still no -- 0.449
  deg is real headroom, just not what is visible here.
- **Not the shading.** Writing the decoded normal straight to the framebuffer
  shows the corduroy in the normal itself.

It is in what the WMS returns: fetching the four GetMaps with `curl` and shading
the raw float32 offline reproduces it before anything of ours touches it. I read
that as flight-line striping in the DTM and said so. **Wrong** -- see below.

### It is the geoplateforme's own reprojection, and the fix is to stop asking for WGS84G

The observation that settled it (the user's): the
`IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE.SHADOW` WMTS layer, a hillshade
of the same product, has no corduroy at all. A hillshade is computed on the native
grid and only the *image* is reprojected, so anything that survives our path but
not that one is introduced between the two.

LIDAR HD is 1 m in Lambert-93. The capabilities list a native layer,
`IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE.LAMB93`, which serves it in
`CRS=EPSG:2154` with no reprojection. Fetching the same ground both ways, warping
both onto one 1 m raster and shading them identically: the Lambert grid is clean,
the WGS84G grid is woven. Measured over 500 x 500 m:

| comparison | height | normal |
| --- | --- | --- |
| WGS84G against the native grid | 0.156 m RMS | **3.06 deg RMS** |
| WGS84G against a prefiltered reference resample | 0.174 m | **4.39 deg** |
| our own plain *bilinear* reprojection against the same reference | 0.020 m | **0.82 deg** |

So a naive bilinear reprojection done by us is five times better than what the
service returns. The error field peaks at a 2.72 m wavelength running -16 deg,
which is the second harmonic of the beat between the 1 m Lambert grid and the
WGS84G level-16 grid it is resampled onto (1.193 m N-S, 0.846 m E-W: beats of
6.18 m and 5.49 m). Whatever the server does, it is not filtering.

Two things follow. The 3-4 deg it adds is four times the 0.87 deg bar, which is
why it is visible at all; and it is *not* a reason to stay at 2.38 m -- the
2 x 2 box mean only attenuates it. Our 1024-sample request lands exactly on WMTS
level 16 (`5062.5 / 2^(L+1)` arcsec, so `L = 16`), which is also why the 512 and
1024 outputs are exactly box-consistent: the pyramid is theirs, and we have been
sampling it at its deepest level.

The change: request `...ELEVATIONGRIDCOVERAGE.LAMB93` in EPSG:2154 at 1 m, on a
bbox aligned to a fixed Lambert grid so nearby locations still share URLs (a
1024 m grid is as deterministic as today's footprint rule, and dyadic in metres),
and resample it ourselves in the blend. Costs: a Lambert-93 forward projection
(~40 lines, GRS80 LCC 2SP, the constants are fixed), and a resample from a grid
that is rotated by the meridian convergence (2.37 deg at 6.36 E) rather than
axis-aligned -- the mapping is smooth, so a per-row linear approximation is exact
to well under a sample and no transcendental need run per sample. ~17 MB for the
current inner extent at 1 m against 16 MB today. Unmeasured: how much of this the
4.77 m ring and l13 carry.

**One projected grid will not do for every territory, and does not need to.**
`...LAMB93` advertises `EPSG:2154` alone, over -5.16..9.57 E / 41.33..51.09 N:
metropolitan France only. La Reunion has its own native layer,
`...ELEVATIONGRIDCOVERAGE.RGR92UTM40S`, `EPSG:2975` alone, 55.21..55.84 E /
-21.39..-20.86. Checked there too, at Piton des Neiges: the native UTM grid is
clean and the WGS84G one carries a heavy rectangular stripe grid -- the same
defect, worse than in the Alps.

What makes this cheap is that every one of these projections is conformal, so
over a ring the map from the projected metre grid to the world tangent frame is a
*similarity*: rotation by the grid convergence, times a uniform scale. The
renderer, the blend, visibility and POI anchoring therefore only ever need a
rotation, a scale and an origin per ring, and never learn which projection
produced them. Per territory the new code is a layer name, a CRS code, and one
`forward : lat -> lon -> x * y` -- LCC 2SP and Transverse Mercator, both on
GRS80, twenty to thirty lines each. Convergence and scale come from
finite-differencing that forward at the ring centre; no analytic formula for
either is needed.

The similarity holds to far below a sample. At 6.36 E the convergence is 2.37 deg
and varies by 0.008 deg across the ring, i.e. 0.17 m at its edge, with the scale
factor moving by ~1e-6; at 55.48 E in zone 40 (central meridian 57 E) the
convergence is 0.54 deg and varies by centimetres over the same span.

Two consequences worth having in mind before writing it. If **both** LIDAR rings
live in the same projected frame, the inner ring's blend onto the middle one stays
axis-aligned and nothing in its addressing changes; only the outer LIDAR ring's
blend onto l13 crosses frames, which is exactly the fade the annulus already
exists to hide. And that one blend loses the row-pair cache -- 2.4 deg over 2048
samples drifts 86 source rows across a single output row -- though an affine map
still gives constant per-sample increments, so the loop stays incremental. The
grid also becomes square, 1 x 1 m instead of 1.19 x 0.85, which makes the relief
Sobel isotropic. The native layers advertise `MinScaleDenominator` 1785.7, i.e.
0.5 m per pixel, so 1 m is well inside what they serve.

### Implementing it

Measured before starting, each candidate against a dense 36-tap reference on the
same grid, so this is resampling error alone and nothing else:

| | height | normal |
| --- | --- | --- |
| IGN WGS84G, what we fetch today | 0.172 m | **4.29 deg** |
| our own plain bilinear | 0.011 m | 0.44 deg |
| our own 4-tap box | 0.009 m | 0.39 deg |
| our own 9-tap box | 0.006 m | **0.25 deg** |

So reprojecting ourselves onto the existing graticule-aligned ring would recover
about 94% of what is there, and keeping the ring on the projected grid recovers
the rest and makes it square. The second was chosen. The two turned out to cost
about the same, for a reason worth recording: **only one of the three blends
crosses frames.** l13 over the base stays lat/lon to lat/lon; the inner LIDAR
ring over the middle one is projected to projected in the *same* CRS, hence still
axis-aligned; only the middle ring over l13 mixes the two. So `blend.wat` needs
an added path rather than surgery, and its existing one stays bit-identical and
still serves two blends out of three. Reprojecting in `Hd_dem` instead would have
needed a resample kernel of its own -- 4M samples cannot run in OCaml on the
fetch path, the blend's own resample being 448 ms there against 13.5 ms in wat.

Staged so that every step is verifiable on its own:

1. **`Projection`** (`src/lib`): LCC 2SP for Lambert-93, Transverse Mercator for
   RGR92 / UTM 40S, both GRS80, plus the local Jacobian by differencing the
   forward. Adding a territory is a forward projection and a bounding box.
   *Done.* Checked against an independent implementation: 966189.32 / 6409476.24
   at 44.73339,6.36308 and 341921.30 / 7666150.88 at Piton des Neiges,
   convergence 2.44 deg and 0.55 deg, negative west of the central meridian, and
   `None` outside both territories.
2. **`Affine`** (`src/lib`) and **`Hd_dem.frame`**: one affine both ways between
   arcseconds from the anchor and a fractional sample index, replacing the
   origin-and-spacing every consumer was computing with. *Done*, along with the
   six consumers -- eye height, POI anchoring, visibility, the relief bake's
   spacing, and `hd_scale` becoming a `mat2` in `radial_common.vert`. Verified as
   a no-op: **RMSE 0 and zero differing pixels** over the terrain at
   44.73339,6.36308, with all three rings loading and blending to the same ranges
   and steps as before. (The only difference in the frame was the service
   worker's own "new version available" toast.)
3. **`Blend_core`**: general affine addressing, with `Blend_wasm` using the
   existing wat when the map is axis-aligned and the OCaml path otherwise. That
   keeps `blend.wat` out of the critical path entirely -- the one rotated blend is
   1024^2, which the OCaml path did in ~65 ms before it was ported. A rotated wat
   path is then an optimisation to take or leave.
4. **`Hd_dem`**: the projected layer kind -- bbox in the CRS's own metres aligned
   to a fixed metre grid so URLs stay shared and cacheable, frame from the
   Jacobian at the anchor. *Done.*
5. Switch the two LIDAR rings over. *Done.*

### Verifying it, and one real bug found on the way

The two LIDAR rings now fetch `...ELEVATIONGRIDCOVERAGE.LAMB93` in EPSG:2154 and
the corduroy is gone. Everything up to the grid is verified:

- the four GetMaps return real data, 0% nodata, ranges matching what `curl`
  returns for the same bboxes;
- the blended grid's mean neighbour difference equals the samples' to four
  decimals (1.6381 vs 1.6379 m on the middle ring), so the refinement is in the
  grid and the fade is not eating it;
- the wat and the OCaml reference agree on every blend, and the new general
  (turned-axes) loop in `Blend_core` is **byte-identical** to the axis-aligned one
  when given a diagonal map, checked natively.

The render came out smoother, which looked like a regression and took a long
chase to disprove. Ruled out along the way, each with a capture -- recorded
because every one of them is a place a future change could genuinely break:

- **not coverage.** First suspicion, and a real bug that was fixed: an alignment
  grid of half the span let the anchor sit a quarter-span off centre, and the
  middle ring then reached only 1292 m north of a camera looking north. Now a
  quarter-span, and the middle ring is 5 m x 1024 for at least 1920 m of reach
  against the 1832 m worst case of the ring it replaces. The detail did not come
  back, and the nearest ground was never outside the inner ring anyway.
- **not the blend, the fetch or the bake input**, per the three checks above.
- **not the ring selection.** Colouring fragments by which slot they take shows
  the rings being chosen exactly where they should be, and the normalised
  coordinates ramp across the view at the right rate.
- **not the relief bake's spacing.** It gets (0.997, 1.000) m and (4.988, 5.001)
  m, against (21.9, 30.9) x px_arcsec before -- same magnitude.

- **not the level the pyramid is read at.** Instrumented: `lod_raw` and the
  chosen level passed out of `computeRadialVertex` as a varying, plus the
  fragment's own automatic LOD from the derivative of `hdReliefCoord`. Down the
  middle of the frame:

  | screen y | slot | `lod_raw` | level | ground | fragment LOD |
  | --- | --- | --- | --- | --- | --- |
  | 780-600 | 0 (1 m) | -3.51 | 1 | 2 m | 0 |
  | 540 | 0 | -2.50 | 2 | 4 m | 0.5 |
  | 480 | 0 | -2.44 | 2 | 4 m | 0.2 |
  | <=420 | 1 (5 m) | <=-2.38 | 0 | 5 m | 0 |

  Correct, and *finer* than what it replaces: at the bottom the old ring computes
  `max(0, -3.51 + 3.695) = 0.18`, i.e. level 0 of a 2.38 m grid, against 2 m now;
  at y=480 it gets 4.77 m against 4 m. `hd_lod_bias` and `arcsec_step` are
  exonerated -- they were the leading hypothesis and they are wrong.

- **not the bake.** Read back out of the GPU rather than inferred: a framebuffer
  over each baked texture, `readPixels` of the central 256^2, decoded.

  | ring | input tile | baked height level 0 | normal sd (x, y) |
  | --- | --- | --- | --- |
  | inner, 1 m | 0.650 m mean \|dx\| | **0.650 m** | 0.456, 0.193 |
  | middle, 5 m | 1.795 m | **1.795 m** | 0.349, 0.256 |
  | l13, 9.5 m | 2.414 m | **2.414 m** | 0.338, 0.350 |

  The height pyramid reproduces its input exactly and the normal textures carry
  strong relief. Nothing is being smoothed.

### There was no regression: the detail that went missing was IGN's noise

Every link measured correct, which was the cue to doubt the premise rather than
keep bisecting. Comparing the two products directly on the same ground, over the
600^2 centre of the same footprint:

| | mean \|dh\|/m, across / along | residual after a 3-tap smooth |
| --- | --- | --- |
| native Lambert, 1 m | 0.2180 / 0.2503 -> 12.30 / 14.05 deg | **0.0518 m rms** |
| IGN WGS84G | 0.2215 / 0.2733 -> 12.49 / 15.29 deg | **0.0790 m rms** |

The real terrain slope is the same in both. What the WGS84G grid has and the
native one does not is **52% more energy at the very finest scale** -- which is
exactly the corduroy, 0.172 m of height error and 4.29 degrees of normal error.
So the render is smoother because the noise that was reading as detail is gone,
and no real detail was lost. Same lesson as the procedural bump masking the
1.19 m data, running the other way: a high-frequency artefact reads as
resolution.

Two cautions worth keeping. Screen-space high-pass energy is **not** a measure of
detail -- it scored l13 alone above the 5 m ring, because facet edges and
quantisation steps are high-frequency too; compare grids, not renders. And a warm
capture profile serves the previous `blend.wasm` from the service worker's app
cache while running the new viewer, a silent parameter mismatch that produced a
5 889 564 m height range before it was noticed. The capture script now drops the
`mountains-*` caches and keeps `v1`, so tiles stay warm and the shell does not.

What is genuinely new and visible is **faceting** on smooth near ground, which
the corduroy was hiding. It belongs with the procedural bump's amplitude as an
appearance item, not with this change.

### The white sawtooth on a shadowed crest is not the shadow map

Reported at 44.73339,6.36308 alpha=-129 beta=86 zoom=3.00: a bright zigzag
running along a ridge inside a dark slope. Two measurements settle what it is.

**It is not the shadow.** Rendering `shadow_val` alone shows it at essentially 1
across the whole region -- nothing there is shadowed at all. The dark slope is
the diffuse term, facing away from a low sun, and the bright line is where the
terminator crosses a crest.

**It is new.** Building f3fda0d into a separate build dir and capturing the same
view from it: no trace of the line. So this one is not "the boundaries moved and
it was always there" -- unlike the ring seams, where that is exactly right.

What changed is the relationship between the mesh and the normal map. The vertex
LOD instrumentation from the last round measured the near field reading **level 1
of the 1 m ring, i.e. 2 m**, while the fragment normal reads **level 0, 1 m**.
Before, the ring was 2.38 m and both read its level 0 -- geometry and shading at
the same scale. Now the shading is twice as fine as the geometry, so each 2 m
facet carries normals that vary across it; at a grazing sun that turns the
terminator into a sawtooth of lit facets. The crest also has to be crisp for it
to show, which the 1 m data makes it and the 1.19 m WGS84G data, with its
corduroy, did not.

So it is the same family as the faceting noted above -- geometry coarser than
what is shaded on it -- with the sun angle making it obvious. Fixes worth
weighing: let the mesh read the level the normals do near the camera; or accept
that a ring finer than the mesh is normal mapping and soften the terminator.
Not addressed here.

### Seams at the ring boundaries

Reported after the switch, and worth separating into three things.

**Heights are continuous to the last bit.** At sample 0 the edge fade is exactly
zero, so the blended grid must equal the source's bilinear upsample there; it
does, to mean 0.9 cm and max 1.7 cm, which is half a quantum of each grid
(3.46/2, 2.25/2, 1.03/2). Nothing to fix.

**One real bug: the ring's normals were turned from north.** `normal.frag`
differentiates along the grid's own axes and `terrain.frag` reads the result as
east and north, which is the same thing only for a grid on the graticule. A
projected ring's normals came out rotated by the CRS's grid convergence -- 2.44
degrees at 6.36 E -- against everything beyond its edge. Fixed by handing the
bake the grid's axes as east/north displacements rather than two step lengths,
and rotating the gradient back; the matrix is the identity for the base and l13,
so their bake is untouched. Measured as the angle between the normal a grid gives
at its border and the one the surface beneath gives at the same ground point,
interpolated the way the renderer's filtered fetch does:

| boundary | before | after |
| --- | --- | --- |
| 5 m ring over l13 (projected over graticule) | 0.99 deg | **0.288 deg** |

Inside the 0.87 degree bar, from twice over it. The whole render moves by mean
0.65 grey levels, max 4 -- the right size for 2.44 degrees.

**One that remains, and is not what it looks like: 1.29 deg mean at the 1 m over
5 m boundary** (max 22 deg). Both grids are in the same CRS, so the rotation fix
does not touch it. The obvious suspect was the resolution ratio, which this
change took from 2.38:4.77 to 1:5 -- **tested and wrong, twice**: at 1:2.5 the
inner seam moves only 1.292 -> 1.242 deg, while the outer one gets *worse*,
0.288 -> 0.629 deg, because the middle ring then sits 3.8x from l13 instead of
1.9x. Reverted to 5 m.

What separates the two boundaries is not the ratio but what the coarse surface
holds. Where the 5 m ring fades into l13, l13 at 9.5 m is smooth and there is
nothing to disagree about. Where the 1 m ring fades into the 5 m one, the coarse
surface carries real LIDAR detail, and the two describe it differently: inside,
a 1 m difference of the bilinear upsample; outside, a 5 m difference of the grid
itself. Fading to a bilinear upsample makes the heights agree, and cannot make
the derivatives agree, because differentiating an interpolant at a finer scale is
not the same operator. That is a property of the design rather than of this
change -- the same mismatch existed at 2.38 over 4.77 -- and the corduroy was
covering it.

Options, none tried: upsample the source with a smoother kernel than bilinear
near the border, so its 1 m derivative matches the coarse one; or blend the
normals over a narrow band in the shader instead of switching hard, which is the
honest admission that the fade cannot reconcile derivatives. A caution for
whoever measures this next: comparing the fine normal against the *nearest*
coarse sample rather than an interpolated one measures terrain curvature over
the offset and reads about 2.2 deg whatever is going on.

A caution learned twice over: a warm capture profile serves the previous
`blend.wasm` from the service worker's app cache while running the new viewer,
which is a silent parameter mismatch and produced a 5 889 564 m height range
before it was noticed. The capture script now drops the `mountains-*` caches and
keeps `v1`, so the tiles stay warm and the shell does not.



Sizing at step 4: 1 m square, 2048 samples for the inner ring (2048 m per side,
against +-1.22 x 0.87 km today) and 1024 at 4 m for the middle (4096 m, against
+-2.44 x 1.70 km). Alignment 1024 m and 2048 m, both exact in the URL. Same
number of GetMaps and the same bytes as today.

### Order of work

1. **Move the LIDAR rings to the LAMB93 layer** and reproject in the blend. This
   is what the user is seeing today, and it makes 1.19 m worth having rather than
   a trade.
2. **Reduce the procedural bump's amplitude at close range.** `ac8ddc5` fixed its
   aliasing, not its dominance; the amplitude is an aesthetic choice.
3. Tightening the blend's range is **closed**, measured: no slack to recover.

### The streaks between the rings and the crest sawtooth, isolated (2026-08-06)

Reported at 44.73339,6.36308: strong smeared artifacts "between the 5 m and 1 m
rings" (alpha=24 beta=65 zoom=2.54) and faceting inside the 1 m ring
(alpha=-129 beta=86 zoom=3.00). Both reproduced headlessly with `Date.now`
frozen to 2026-07-06 08:00 UTC — a night-time visit takes the sun fallback
(`viewer.ml` swaps in month=July, 10:00 local, keeping the day), so that one
timestamp reproduces the lighting of an evening screenshot exactly. Two debug
builds: one wrote `shadow_val` / `cosTheta` (geometric N·L) / `final_l` into
R/G/B of one capture; the other tinted fragments by the ring slot the normal
comes from.

- **The shadow map is blameless in both views.** `shadow_val` is essentially 1
  over the whole streaked region and along the sawtooth crest (the sawtooth is
  lit facets at the terminator, as recorded in abb0e38).
- **All of the streaking is in the relief normal field** — `cosTheta` alone
  carries it pixel-for-pixel. The dark bands are real couloirs running down
  the slope, elongated by the grazing view; in the 5 m ring they are magnified
  several-fold transversally at this zoom, so they render as soft airbrushed
  smears directly against pixel-crisp 1 m ground. A resolution cliff, not a
  rendering defect.
- **The 1 m ring's boundary runs along the reported crest at this alpha**
  (slot tint), which is why the cliff reads as "an artifact between the
  rings": the worst smears sit on ground just past the crest, seen
  near-tangent, where every metre of transverse resolution shows.
- The detail map is still disabled (2aa56ad, `if false` in
  `load_compressed_detail_map`), so both artifacts are being judged naked; by
  the f3fda0d lesson the texture/bump masks much of the 5 m smoothness.

Options, none tried, for the cliff: re-enable the textures and re-judge before
spending anything (the disable is temporary instrumentation); scale the
procedural bump with the local data footprint so synthetic detail fills the
5 m ring and yields inside the 1 m one; a finer middle ring (5 m → 2–2.5 m,
~4x its bytes) if real detail is wanted at telephoto zooms. For the sawtooth
family (shading finer than the mesh): damp `direct` by a geometry-scale N·L
(computed per vertex at the level the mesh reads, one varying) — coarse
self-shadowing, but it also dims honestly grazing-lit ridges; or let the
vertex stage read one level finer near the camera and accept the
undersampling risk (static camera makes it worth an experiment).

### The bump now yields to the data that outresolves it (2026-08-07, uncommitted)

With the detail map re-enabled (2aa56ad reverted in the working tree) the
texture masked the ring artifacts and buried the 1 m relief with them. The
bump's job is to stand in for relief the data cannot hold, so it now scales
with the sample pitch of whatever the fragment's normal comes from:
`hd_params` gained `hd_step_m` (the coarser axis of `frame.step_{x,y}_m`),
`Render_state.hd_bump_scale` maps it to `min 1 (step/8)` — 1 m ring 0.125,
5 m ring 0.625, l13 and base 1.0 — and the fragment multiplies the factor
(`hd_bump[slot]`, picked in the existing normal-selection chain) into
`bumpStrength`. Verified headlessly at the two reported views, rings
confirmed in-frame: the 1 m rock bands, gullies and crest read through where
before there was only texture speckle; ground outside the LIDAR rings is
**bit-identical (RMSE exactly 0)** by construction (factor 1.0). The 8 m knee
and the linear ramp are aesthetic choices to tune on a real GPU — SwiftShader
renders the BC7 detail map far more harshly than hardware at grazing
telephoto views, so judge amplitudes on device, only structure here. One
harness note: the GPU timing logs flush lazily (queries resolve on later
frames), so `compute_relief_hd` counts in the console are not a readiness
signal — and neither is `draw_shadows`, for the same reason. The synchronous
"RGE ALTI blended in" lines are the signal: three of them (l13 + both LIDAR
rings) mean the publishes are imminent. **Caveat discovered the next day**:
the view-1 "before" capture of this section had only l13 published (see the
dropped-publish race below), so its dramatic before/after overstated the
bump factor's effect; the factor itself and the far-field RMSE 0 stand.

### Rings that arrive during the first publish were never shown (2026-08-07, uncommitted)

Found because headless captures of the same view kept splitting into two
populations: runs with all three "RGE ALTI blended in" lines, and runs with
exactly one (l13), where the 1 m and 5 m rings never appeared — not late,
*never*, still absent at 125 s with every tile fetched. The refine loop in
`load_location` began with `List.filter still_pending hd_fetches` and
returned when nothing was pending. But the first publish is synchronous and
spans awaits (its GPU bakes take seconds on a slow device), so a ring whose
fetch completed during that window was already settled — not pending — when
`refine` first ran: with no other ring in flight the loop exited without ever
publishing it. Fixed by restructuring `refine` to first publish anything
settled beyond `shown`, and only then wait (`refine`/`wait` mutual
recursion, same epoch discipline). The window is the whole first-publish
duration, so a phone with a fast network and slow GPU hits this on real
loads; it is not a SwiftShader artefact — SwiftShader only widened it to
~50% of runs.

### The seam and the smooth-to-harsh switch at the ring borders (2026-08-07, uncommitted)

Reported at alpha=-146 beta=83 zoom=3 (seam line despite the texture) and
alpha=31 beta=69 zoom=3 (hard switch from the 1 m ring's quiet surface to the
5 m ring's busy one). Both are the same defect: the normal source *and* the
per-ring bump factor switched hard on the extent test. Now `ringFade` gives
each ring a weight — 1 inside, fading to 0 over the outer 2% of its extent —
and both `encodedN` and `bump_scale` composite outside-in through it. The
common case still costs one fetch (fast paths for weight exactly 1 with all
finer rings at 0); only border-band fragments walk the mix chain. Verified
at both reported views with all rings confirmed blended in-frame: the razor
line is gone in both, differences confined to the bands (RMSE 1.3% / 4.6%
against a zero-width-band control from the same build).

Two constants to tune on a real GPU, both deliberate first guesses: the band
width (0.02 of each extent — a 6% band measured as swallowing the *entire*
visible ring-0 area at the reported telephoto views, where all visible fine
ground sits within ~130 m of the ring edge; 2% ≈ 41 m ≈ 15-40 px there) and
the `min 1 (step/8)` bump mapping. The remaining texture-character contrast
between rings is the data-resolution difference itself, spread over the band
rather than a line; if it still reads as a defect on device, lower the 5 m
ring's bump share before touching the band.

### The race fix had the same hole one publish deeper (2026-08-13)

After the rebase onto main the near field rendered flat: the inner ring's
blend never ran (log signature: all tiles fetched, blends stop before the
1 m ring, "eye ... on a 4.98 m grid"). Not a rebase regression — the refine
loop replayed byte-identical, and the ring pipeline files match the
pre-rebase tip exactly. Instrumenting refine showed the sequence: publish
the settled rings, and while that publish runs its seconds of GPU work the
last fetch settles; the publish branch then went back to the *wait* — whose
pending filter finds nothing pending and exits. The 2026-08-07 fix closed
this window for the first publish and left it open for every refine
publish. Fixed in ac438b0: the publish branch re-enters [refine] (the
settled check) instead of [wait]; terminates because [shown] grows
strictly per publish. Verified twice headlessly: 4 blends, eye on the
1.00 m grid, near-field relief back. What exposed it was WMS timing — the
inner ring landing ~2 s behind the others, inside the publish window — so
it would have struck any device with that fetch spacing, rebase or not.
