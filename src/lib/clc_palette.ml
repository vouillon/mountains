(* clc_palette.ml - CORINE Land Cover Palette Generation *)

open Bigarray

(* CLC code to dense index mapping (0-44) *)
(* Official CORINE codes mapped to palette texture indices *)

type material = {
  code : int;
  albedo : int * int * int; (* RGB 0-255, approximately linear *)
  roughness : float; (* 0.0-1.0 *)
  detail_rock : float; (* Red channel weight 0-1 *)
  detail_grass : float; (* Green channel weight 0-1 *)
  detail_forest : float; (* Blue channel weight 0-1 *)
  water_factor : float; (* For shoreline blending 0-1 *)
}

(* 45 material definitions covering all CORINE classes *)
let materials =
  [|
    (* 0: Fallback/Unknown *)
    {
      code = 0;
      albedo = (180, 50, 180);
      roughness = 0.5;
      detail_rock = 0.5;
      detail_grass = 0.5;
      detail_forest = 0.0;
      water_factor = 0.0;
    };
    (* Artificial Surfaces (111-142) *)
    (* 1: Continuous urban fabric *)
    {
      code = 111;
      albedo = (130, 125, 120);
      roughness = 0.7;
      detail_rock = 0.9;
      detail_grass = 0.1;
      detail_forest = 0.0;
      water_factor = 0.0;
    };
    (* 2: Discontinuous urban fabric *)
    {
      code = 112;
      albedo = (160, 155, 145);
      roughness = 0.6;
      detail_rock = 0.6;
      detail_grass = 0.3;
      detail_forest = 0.1;
      water_factor = 0.0;
    };
    (* 3: Industrial or commercial units *)
    {
      code = 121;
      albedo = (140, 130, 140);
      roughness = 0.75;
      detail_rock = 0.95;
      detail_grass = 0.05;
      detail_forest = 0.0;
      water_factor = 0.0;
    };
    (* 4: Road and rail networks *)
    {
      code = 122;
      albedo = (100, 95, 90);
      roughness = 0.8;
      detail_rock = 1.0;
      detail_grass = 0.0;
      detail_forest = 0.0;
      water_factor = 0.0;
    };
    (* 5: Port areas *)
    {
      code = 123;
      albedo = (110, 105, 105);
      roughness = 0.7;
      detail_rock = 0.9;
      detail_grass = 0.0;
      detail_forest = 0.0;
      water_factor = 0.1;
    };
    (* 6: Airports *)
    {
      code = 124;
      albedo = (140, 140, 140);
      roughness = 0.7;
      detail_rock = 0.95;
      detail_grass = 0.05;
      detail_forest = 0.0;
      water_factor = 0.0;
    };
    (* 7: Mineral extraction sites *)
    {
      code = 131;
      albedo = (150, 145, 135);
      roughness = 0.85;
      detail_rock = 1.0;
      detail_grass = 0.0;
      detail_forest = 0.0;
      water_factor = 0.0;
    };
    (* 8: Dump sites *)
    {
      code = 132;
      albedo = (135, 125, 100);
      roughness = 0.9;
      detail_rock = 0.7;
      detail_grass = 0.2;
      detail_forest = 0.1;
      water_factor = 0.0;
    };
    (* 9: Construction sites *)
    {
      code = 133;
      albedo = (160, 150, 130);
      roughness = 0.85;
      detail_rock = 0.8;
      detail_grass = 0.1;
      detail_forest = 0.1;
      water_factor = 0.0;
    };
    (* 10: Green urban areas *)
    {
      code = 141;
      albedo = (60, 120, 45);
      roughness = 0.9;
      detail_rock = 0.1;
      detail_grass = 0.8;
      detail_forest = 0.1;
      water_factor = 0.0;
    };
    (* 11: Sport and leisure facilities *)
    {
      code = 142;
      albedo = (70, 130, 55);
      roughness = 0.85;
      detail_rock = 0.1;
      detail_grass = 0.85;
      detail_forest = 0.05;
      water_factor = 0.0;
    };
    (* Agricultural Areas (211-244) *)
    (* 12: Non-irrigated arable land *)
    {
      code = 211;
      albedo = (170, 160, 85);
      roughness = 0.9;
      detail_rock = 0.1;
      detail_grass = 0.85;
      detail_forest = 0.05;
      water_factor = 0.0;
    };
    (* 13: Permanently irrigated land *)
    {
      code = 212;
      albedo = (100, 140, 65);
      roughness = 0.88;
      detail_rock = 0.05;
      detail_grass = 0.9;
      detail_forest = 0.05;
      water_factor = 0.0;
    };
    (* 14: Rice fields *)
    {
      code = 213;
      albedo = (90, 130, 70);
      roughness = 0.85;
      detail_rock = 0.0;
      detail_grass = 0.9;
      detail_forest = 0.1;
      water_factor = 0.3;
    };
    (* 15: Vineyards *)
    {
      code = 221;
      albedo = (120, 100, 50);
      roughness = 0.8;
      detail_rock = 0.2;
      detail_grass = 0.7;
      detail_forest = 0.1;
      water_factor = 0.0;
    };
    (* 16: Fruit trees and berry plantations *)
    {
      code = 222;
      albedo = (85, 115, 50);
      roughness = 0.85;
      detail_rock = 0.1;
      detail_grass = 0.5;
      detail_forest = 0.4;
      water_factor = 0.0;
    };
    (* 17: Olive groves *)
    {
      code = 223;
      albedo = (95, 110, 55);
      roughness = 0.8;
      detail_rock = 0.15;
      detail_grass = 0.45;
      detail_forest = 0.4;
      water_factor = 0.0;
    };
    (* 18: Pastures *)
    {
      code = 231;
      albedo = (90, 125, 65);
      roughness = 0.9;
      detail_rock = 0.05;
      detail_grass = 0.9;
      detail_forest = 0.05;
      water_factor = 0.0;
    };
    (* 19: Annual crops + permanent crops *)
    {
      code = 241;
      albedo = (140, 145, 70);
      roughness = 0.88;
      detail_rock = 0.1;
      detail_grass = 0.7;
      detail_forest = 0.2;
      water_factor = 0.0;
    };
    (* 20: Complex cultivation patterns *)
    {
      code = 242;
      albedo = (135, 140, 65);
      roughness = 0.88;
      detail_rock = 0.1;
      detail_grass = 0.75;
      detail_forest = 0.15;
      water_factor = 0.0;
    };
    (* 21: Land principally agricultural *)
    {
      code = 243;
      albedo = (110, 130, 55);
      roughness = 0.88;
      detail_rock = 0.1;
      detail_grass = 0.6;
      detail_forest = 0.3;
      water_factor = 0.0;
    };
    (* 22: Agro-forestry areas *)
    {
      code = 244;
      albedo = (95, 120, 50);
      roughness = 0.85;
      detail_rock = 0.1;
      detail_grass = 0.45;
      detail_forest = 0.45;
      water_factor = 0.0;
    };
    (* Forests and Semi-Natural Areas (311-335) *)
    (* 23: Broad-leaved forest *)
    {
      code = 311;
      albedo = (40, 90, 35);
      roughness = 0.95;
      detail_rock = 0.05;
      detail_grass = 0.25;
      detail_forest = 0.7;
      water_factor = 0.0;
    };
    (* 24: Coniferous forest *)
    {
      code = 312;
      albedo = (35, 75, 40);
      roughness = 0.95;
      detail_rock = 0.05;
      detail_grass = 0.15;
      detail_forest = 0.8;
      water_factor = 0.0;
    };
    (* 25: Mixed forest *)
    {
      code = 313;
      albedo = (35, 80, 35);
      roughness = 0.95;
      detail_rock = 0.05;
      detail_grass = 0.2;
      detail_forest = 0.75;
      water_factor = 0.0;
    };
    (* 26: Natural grasslands *)
    {
      code = 321;
      albedo = (85, 130, 55);
      roughness = 0.9;
      detail_rock = 0.1;
      detail_grass = 0.85;
      detail_forest = 0.05;
      water_factor = 0.0;
    };
    (* 27: Moors and heathland *)
    {
      code = 322;
      albedo = (110, 115, 75);
      roughness = 0.88;
      detail_rock = 0.2;
      detail_grass = 0.7;
      detail_forest = 0.1;
      water_factor = 0.0;
    };
    (* 28: Sclerophyllous vegetation *)
    {
      code = 323;
      albedo = (100, 110, 60);
      roughness = 0.85;
      detail_rock = 0.15;
      detail_grass = 0.55;
      detail_forest = 0.3;
      water_factor = 0.0;
    };
    (* 29: Transitional woodland-shrub *)
    {
      code = 324;
      albedo = (90, 115, 50);
      roughness = 0.88;
      detail_rock = 0.1;
      detail_grass = 0.4;
      detail_forest = 0.5;
      water_factor = 0.0;
    };
    (* 30: Beaches, dunes, sands *)
    {
      code = 331;
      albedo = (200, 190, 165);
      roughness = 0.75;
      detail_rock = 0.6;
      detail_grass = 0.35;
      detail_forest = 0.05;
      water_factor = 0.0;
    };
    (* 31: Bare rocks *)
    {
      code = 332;
      albedo = (125, 120, 115);
      roughness = 0.65;
      detail_rock = 0.95;
      detail_grass = 0.05;
      detail_forest = 0.0;
      water_factor = 0.0;
    };
    (* 32: Sparsely vegetated areas - patchy mix of rock and grass *)
    {
      code = 333;
      albedo = (130, 135, 100);
      (* Greener tint for visible grass *)
      roughness = 0.75;
      detail_rock = 0.45;
      (* Reduced rock for more balance *)
      detail_grass = 0.50;
      (* Increased grass for patchy appearance *)
      detail_forest = 0.05;
      water_factor = 0.0;
    };
    (* 33: Burnt areas *)
    {
      code = 334;
      albedo = (50, 45, 40);
      roughness = 0.9;
      detail_rock = 0.7;
      detail_grass = 0.2;
      detail_forest = 0.1;
      water_factor = 0.0;
    };
    (* 34: Glaciers and perpetual snow - ICE LOGIC trigger *)
    {
      code = 335;
      albedo = (245, 248, 255);
      roughness = 0.25;
      detail_rock = 0.0;
      detail_grass = 0.0;
      detail_forest = 0.0;
      water_factor = 0.0;
    };
    (* Wetlands (411-423) *)
    (* 35: Inland marshes *)
    {
      code = 411;
      albedo = (75, 105, 70);
      roughness = 0.9;
      detail_rock = 0.05;
      detail_grass = 0.85;
      detail_forest = 0.1;
      water_factor = 0.4;
    };
    (* 36: Peat bogs *)
    {
      code = 412;
      albedo = (80, 85, 60);
      roughness = 0.92;
      detail_rock = 0.1;
      detail_grass = 0.8;
      detail_forest = 0.1;
      water_factor = 0.3;
    };
    (* 37: Salt marshes *)
    {
      code = 421;
      albedo = (120, 125, 100);
      roughness = 0.85;
      detail_rock = 0.2;
      detail_grass = 0.7;
      detail_forest = 0.1;
      water_factor = 0.5;
    };
    (* 38: Salines *)
    {
      code = 422;
      albedo = (185, 180, 175);
      roughness = 0.7;
      detail_rock = 0.6;
      detail_grass = 0.3;
      detail_forest = 0.1;
      water_factor = 0.6;
    };
    (* 39: Intertidal flats *)
    {
      code = 423;
      albedo = (140, 135, 120);
      roughness = 0.8;
      detail_rock = 0.5;
      detail_grass = 0.4;
      detail_forest = 0.1;
      water_factor = 0.7;
    };
    (* Water Bodies (511-523) *)
    (* 40: Water courses *)
    {
      code = 511;
      albedo = (25, 75, 130);
      roughness = 0.1;
      detail_rock = 0.0;
      detail_grass = 0.0;
      detail_forest = 0.0;
      water_factor = 1.0;
    };
    (* 41: Water bodies *)
    {
      code = 512;
      albedo = (20, 70, 135);
      roughness = 0.08;
      detail_rock = 0.0;
      detail_grass = 0.0;
      detail_forest = 0.0;
      water_factor = 1.0;
    };
    (* 42: Coastal lagoons *)
    {
      code = 521;
      albedo = (40, 100, 140);
      roughness = 0.12;
      detail_rock = 0.0;
      detail_grass = 0.0;
      detail_forest = 0.0;
      water_factor = 1.0;
    };
    (* 43: Estuaries *)
    {
      code = 522;
      albedo = (55, 95, 120);
      roughness = 0.15;
      detail_rock = 0.0;
      detail_grass = 0.0;
      detail_forest = 0.0;
      water_factor = 0.95;
    };
    (* 44: Sea and ocean *)
    {
      code = 523;
      albedo = (15, 55, 120);
      roughness = 0.05;
      detail_rock = 0.0;
      detail_grass = 0.0;
      detail_forest = 0.0;
      water_factor = 1.0;
    };
  |]

(* Build lookup table from CLC code to array index *)
let code_to_index =
  let tbl = Hashtbl.create 64 in
  Array.iteri (fun idx m -> Hashtbl.add tbl m.code idx) materials;
  tbl

let get_index code =
  match Hashtbl.find_opt code_to_index code with Some i -> i | None -> 0

(* Generate 128x1 RGBA palette texture (2 pixels per material)
   Pixel A (even): Albedo RGB + Roughness A
   Pixel B (odd): Detail Weights RGB + Water Factor A *)
let generate_palette () =
  let data = Array1.create int8_unsigned c_layout (128 * 4) in
  Array1.fill data 0;
  let n_materials = Array.length materials in
  for idx = 0 to n_materials - 1 do
    let m = materials.(idx) in
    let base = idx * 2 * 4 in
    (* 2 pixels per material, 4 bytes each *)
    let r, g, b = m.albedo in
    (* Pixel A: Albedo + Roughness *)
    Array1.set data (base + 0) r;
    Array1.set data (base + 1) g;
    Array1.set data (base + 2) b;
    Array1.set data (base + 3) (int_of_float (m.roughness *. 255.));
    (* Pixel B: Detail Weights + Water Factor *)
    Array1.set data (base + 4) (int_of_float (m.detail_rock *. 255.));
    Array1.set data (base + 5) (int_of_float (m.detail_grass *. 255.));
    Array1.set data (base + 6) (int_of_float (m.detail_forest *. 255.));
    Array1.set data (base + 7) (int_of_float (m.water_factor *. 255.))
  done;
  data

(* Number of material definitions *)
let n_materials = Array.length materials
