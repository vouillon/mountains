# Terrain Rendering

**Deployed Application**: [https://vouillon.github.io/mountains/](https://vouillon.github.io/mountains/)

A Web-based terrain rendering application capable of visualizing landscapes from Digital Elevation Models (DEMs), currently covering the French Alps. It features a primary WebGL client using `Brr` and a comprehensive data pipeline for processing CORINE Land Cover (CLC) data.

![Col Girardin](assets/col%20Girardin.jpg)

## Key Features

- **Web Client (Primary)**: Written in OCaml, compiled to JavaScript/WASM, utilizing WebGL for high-performance rendering in any modern browser.
- **Native Client (Legacy)**: Support for a desktop application using SDL2 and OpenGLES 3.0 (available for testing/debugging).
- **Advanced Terrain Rendering**:
    - Radial clipmap geometry for seamless level-of-detail.
    - Cascaded shadow maps for realistic lighting.
    - Specialized material system for CORINE Land Cover.
    - Ambient occlusion
- **Data Processing Tools**:
    - **Tile Extraction**: Pipeline to extract and regularize map tiles from GeoPackage databases (`Corine/extract_tiles.exe`).
    - **Polygon Processing**: Robust clipping, splitting, and triangulation of complex land cover polygons.
    - **OSM Integration**: Augments land cover data with high-precision water bodies from OpenStreetMap.
    - **Static R-tree**: Spatial index integration for accelerating polygon triangulation operations.

## Project Structure

The codebase is organized into modular components:

- **`src/lib`** (Core): Platform-agnostic core logic, math libraries (Matrix/Vector), IO abstractions, and terrain algorithms.
- **`src/web`** (Web Client): Primary browser-specific implementation using `Brr`, managing WebGL state, input, and networking.
- **`src/sdl`** (Native Client - Legacy): Desktop implementation using `Tsdl` and `Tgles3` (legacy support).
- **`Corine`** (Tools): A suite of command-line tools for processing geospatial data, polygon validation, and generating tile assets.

## Build Instructions

The project uses `dune` as the build system.

**Build all targets:**
```bash
dune build
```

## Data Sources

1.  **DEM**: 30m resolution Copernicus Digital Elevation Models.
2.  **Land Cover**: CORINE dataset processed into triangulated optimized tiles.
3.  **Water**: OSM water polygons fetched and merged for higher precision coastlines and lakes.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
