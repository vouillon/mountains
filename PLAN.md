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

## Near-field DEM refinement with RGE ALTI — designed around residual encoding (2026-07-31)

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
