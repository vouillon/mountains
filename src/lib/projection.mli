(** The projected coordinate reference systems IGN publishes its LIDAR HD
    elevation in, one per territory.

    The reason this exists: the geoplateforme also offers those products
    reprojected to geographic coordinates, and that reprojection is where the
    corrugation on smooth slopes comes from -- 4.29 degrees of normal error
    against a clean resample of the same data (PLAN.md). Requesting each
    territory in its own native CRS avoids it, at the cost of a grid whose axes
    are not north and east.

    Only what a caller needs to place and read such a grid is here: the layer
    and CRS names for the request, the forward projection to build a bounding
    box with, and the local derivative that turns that grid into an affine map.
    There is no inverse: every consumer works forward, from a geographic offset
    to a sample index. *)

type t

val name : t -> string
(** The geoplateforme layer's CRS suffix, e.g. ["LAMB93"]. *)

val epsg : t -> string
(** The [CRS=] parameter of a WMS request, e.g. ["EPSG:2154"]. *)

val forward : t -> lat:float -> lon:float -> float * float
(** Easting and northing in metres. Defined outside the CRS's area of use, but
    only meaningfully inside it. *)

val jacobian : t -> lat:float -> lon:float -> float * float * float * float
(** [(dx/dlon, dx/dlat, dy/dlon, dy/dlat)] at that point, metres per degree.

    This is what makes a projected grid cheap to use: all of these projections
    are conformal, so the matrix is a rotation times a scale, and over a
    kilometre or two it barely moves -- 0.008 degrees of rotation and about a
    part in a million of scale across a 2.4 km ring in the Alps. A caller can
    therefore treat the whole map from geographic offsets to projected metres as
    this one matrix and be wrong by centimetres, rather than projecting every
    sample.

    Obtained by differencing {!forward}, so adding a CRS means adding a forward
    projection and nothing else. *)

val of_location : lat:float -> lon:float -> t option
(** The CRS whose area of use contains the point, if any. [None] outside every
    territory these products cover, where the caller must fall back to a
    geographic layer. *)
