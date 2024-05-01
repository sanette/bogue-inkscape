(* This file is part of Bogue-inkscape

   Copyright Vu Ngoc San 2024

   This module parses an Inkscape file and produces an intermediate
   representation
*)

(*

#require "xmlm";;
(* eval buffer *)
Sys.chdir "/home/san/prog/ocaml/bogue-inkscape";;
parse "bogue-inkscape.svg";;

*)

let tested_versions = [ (1,3,2) ]

open Printf

(* DPI for which Bogue scale=1 *)
let bogue_dpi = 110.

(* Define types for first pass scanning *)

type unit = Cm | Mm | Q | In | Pc | Pt | Px

type color = int * int * int * int (* RGBA *)

let black = (0,0,0,255)

(* Only Translate is implemented
   https://www.w3.org/TR/SVG/coords.html#TransformProperty *)
type transform =
  | Translate of (float * float)
  | Scale of float

type style = {
    fill : color option;
    stroke_width : float option;
    stroke_color : color option;
    radius : float option;
  }

let no_style = { fill = None; stroke_color = None;
                 stroke_width = None; radius = None }

type rect = {
  id : string;
  label : string option;
  title : string option;
  desc : string option;
  x : float; y : float; w : float; h : float;
  style : style
}

type canvas = {
  viewport : rect;
  width : float;
  height : float;
  xscale : float;
  yscale : float;
}

type tspan = { id : string; x : float; y : float ; text : string }
(* x,y is the base position of the first glyph, usually bottom left point of the
   letter (on the baseline). It's not the top-left corner of the box containing
   the text!*)

type text = {
  id : string;
  color : color;
  font_size : float;
  font_family : string;
  texts : tspan list
}

type version = int * int * int

type obj =
  | Rect of rect
  | Image of (rect * string)
  | Text of text
  | Group of group
and group = rect * (obj list) * (transform list)

type svg = canvas * (obj list) * (version option)
type connection = { src : string; dst : string; action : string option }
type var = string * string

type inkscape =
  | Svg of svg
  | Obj of obj
  | Tspan of tspan
  | Connection of connection
  | Var of var
  | Ignore

let rect_of_pos ?(id = "tmp") x y =
  { id; x; y; w=0.; h=0.;
    label = None; title = None; desc = None; style = no_style }

let find_attr_opt a name =
  List.find_opt (fun ((_, n), _) -> n = name) a
  |>  Option.map snd

let find_attr a name =
  match find_attr_opt a name with
  | Some v -> v
  | None -> failwith (sprintf "Cannot find attribute [%s]" name)

(* Inkscape version *)
let find_version a =
  List.find_opt (fun ((url, n), _) ->
      n = "version" && Filename.basename url = "inkscape") a
  |> Option.map snd
  |> Option.map (fun v -> Scanf.sscanf v "%u.%u.%u" (fun a b c -> (a,b,c)))

let find_float a name =
  float_of_string (find_attr a name)

let find_float_opt a name =
  find_attr_opt a name
  |> Option.map float_of_string

(* https://www.w3.org/TR/SVG/coords.html *)

(* https://www.w3.org/TR/css-values-3/#absolute-lengths *)
(* We use bogue_dpi instead of the traditional 96DPI *)
let parse_dim s =
  match float_of_string_opt s with
  | Some f -> f, Px
  | None ->
    let f, unit =
      try Scanf.sscanf s "%f%s" (fun f s -> f,s)
      with Scanf.Scan_failure _ -> failwith "Malformed length" in
    let unit = match unit with
      | "cm" -> Cm
      | "mm" -> Mm
      | "Q" -> Q
      | "in" -> In
      | "pc" -> Pc
      | "pt" -> Pt
      | "px" -> Px
      | _ -> failwith "Malformed length" in
    f, unit

let convert_dim f unit =
  match unit with
    | Cm -> (f /. 2.54) *. bogue_dpi
    | Mm -> (f /. 25.4) *. bogue_dpi
    | Q -> (f /. 101.6) *. bogue_dpi
    | In -> f *. bogue_dpi
    | Pc -> (f /. 6.) *. bogue_dpi
    | Pt -> (f /. 72.) *. bogue_dpi
    | Px -> f

let parse_viewbox v =
  try
    Scanf.sscanf v "%f %f %f %f" (fun a b c d -> (a,b,c,d))
  with _ -> try
      Scanf.sscanf v "%f , %f , %f , %f" (fun a b c d -> (a,b,c,d))
        with _ -> failwith (sprintf "Malformed viewbox [%s]" v)

let parse_transform s =
  let inch = Scanf.Scanning.from_string s in
  let rec loop res =
    try let tr = Scanf.bscanf inch
            " translate ( %f , %f ) " (fun x y -> Translate (x, y)) in
      loop (tr :: res)
    with _ ->
    try let sc = Scanf.bscanf inch " scale ( %f ) " (fun x -> Scale x) in
      loop (sc :: res)
    with _ -> List.rev res in
  loop []

let global_translate tlist =
  List.fold_left (fun (dx, dy) -> function
      | Translate (a, b) -> (dx +. a, dy +. b)
      | _ -> print_endline "Only translate transforms are supported.";
        (dx, dy)) (0., 0.) tlist

let extract_byte s i =
  int_of_string ("0x" ^ (String.sub s i 2))

let extract_half s i =
  int_of_string ("0x" ^ (String.sub s i 1))

let parse_rbg c =
  if c = "" then begin print_endline "Empty color string"; None end
  else if c.[0] = '#'
  then begin
    if String.length c = 7
    then Some (extract_byte c 1, extract_byte c 3, extract_byte c 5)
    else if String.length c = 4
    then Some (extract_half c 1, extract_half c 2, extract_half c 3)
    else begin
      print_endline (sprintf "Wrong color string: [%s]" c);
      None
    end
  end
  else None

let parse_color opacity c =
  let opacity = Option.value ~default:255 opacity in
  parse_rbg c
  |> Option.map (fun (r,g,b) -> (r,g,b,opacity))

let parse_opacity = function
  | None -> None
  | Some x -> 255. *. (float_of_string x)
              |> Float.round
              |> int_of_float
              |> Option.some

let option_bind f a = Option.bind a f

let style_to_list s =
  String.split_on_char ';' s
  |> List.map (fun param ->
      match String.split_on_char ':' param with
      | [key; value] -> key, value
      | _ -> print_endline (sprintf "Wrong parameter string [%s]" param);
        param, "")

let parse_style ~radius s =
  (* First convert the style string to an assoc List *)
  let map = style_to_list s in
  let opacity = List.assoc_opt "opacity" map
                |> parse_opacity in
  let fill = match List.assoc_opt "fill" map with
    | None -> None
    | Some "none" -> None
    | Some c -> parse_color opacity c in
  let stroke_opacity = List.assoc_opt "stroke-opacity" map
                       |> parse_opacity in
  let stroke_width = List.assoc_opt "stroke-width" map
                     |> Option.map float_of_string in
  let stroke_color = List.assoc_opt "stroke" map
                     |> option_bind (parse_color stroke_opacity) in
  { fill; stroke_color; stroke_width; radius }

let parse_svg a =
  if find_attr_opt a "inkscape" = None
  then failwith "This does not seem to be an Inkscape file";
  let version = find_version a in
  let fwidth, xunit = find_attr a "width" |> parse_dim in
  let fheight, yunit = find_attr a "height" |> parse_dim in
  let x, y, w, h = find_attr_opt a "viewBox"
                   |> Option.get
                   |> parse_viewbox in
  let width = convert_dim fwidth xunit in
  let xscale = width /. w in
  let height = convert_dim fheight yunit in
  let yscale = height /. h in
  let id = find_attr a "id" in
  let label = find_attr_opt a "label" in
  let viewport = { id; label; title = None; desc = None; x; y; w; h;
                   style = no_style } in
  version, { viewport; width; height; xscale; yscale }

let option_to_list = function
  | Some l -> l
  | None -> []

let parse_group a =
  let id = find_attr a "id" in
  let label = find_attr_opt a "label" in
  let mode = find_attr_opt a "groupmode" in
  let smode = match mode with
    | None -> ""
    | Some mode -> sprintf "mode=%s, " mode in
  print_endline (sprintf "Group %sid=%s" smode id);
  let tlist = find_attr_opt a "transform" |> Option.map parse_transform
              |> option_to_list in
  { id; label; title = None; desc = None; x=0.; y=0.; w=0.; h=0.;
    style = no_style },
  [], tlist
(* In fact, we don't need w and h, except for the main svg object *)

let parse_path a : connection option =
  match find_attr_opt a "connection-start" with
  | Some src -> begin
      let dst = match find_attr_opt a "connection-end" with
        | Some dst -> dst
        | None -> src in
      print_endline (sprintf "Connection from [%s] to [%s]" src dst);
      Some { src; dst; action = None }
    end
  | _-> print_endline "ignored Path";
    None

let parse_rect a : rect =
  let id = find_attr a "id" in
  let x = find_float a "x" in
  let y = find_float a "y" in
  let h = find_float a "height" in
  let w = find_float a "width" in
  let ry = find_float_opt a "ry" in
  let style = find_attr_opt a "style"
              |> Option.map (parse_style ~radius:ry)
              |> Option.value ~default:no_style in
  let label = find_attr_opt a "label" in
  let slabel = Option.value ~default:"" label in
  print_endline (sprintf "Rectangle id=%s %s (%f, %f, %f, %f)" id slabel x y w h);
  { id; x; y; w; h; label; title = None; desc = None; style }

let parse_title _a : var =
  ("title", "")

let parse_image a =
  let r = parse_rect a in
  let href = find_attr a "href" in
  r, href

let parse_text a =
  let id = find_attr a "id" in
  let style = find_attr a "style" |> style_to_list in
  let font_family = List.assoc "font-family" style in
  let font_size, unit = List.assoc "font-size" style |> parse_dim in
  let opacity = List.assoc_opt "fill-opacity" style |> parse_opacity in
  let color = List.assoc_opt "fill" style
              |> option_bind (parse_color opacity)
              |> Option.value ~default:black in
  let font_size = convert_dim font_size unit in
  { id; color; font_size; font_family; texts = [] }

let parse_tspan a =
  let id = find_attr a "id" in
  let x = find_float a "x" in
  let y = find_float a "y" in
  { id; x; y; text = "" }

let parse_element a = function
  | "svg" -> let version, canvas = parse_svg a in Svg (canvas, [], version)
  | "g" -> Obj (Group (parse_group a))
  | "rect" -> Obj (Rect (parse_rect a))
  | "path" -> begin
      match parse_path a with
      | None -> Ignore
      | Some p -> Connection p
    end
  | "title" -> Var (parse_title a)
  | "desc" -> Var ("desc", "")
  | "image" -> Obj (Image (parse_image a))
  | "text" -> Obj (Text (parse_text a))
  | "tspan" -> Tspan (parse_tspan a)
  | _ -> Ignore

let get_vars list =
  let rec loop acc = function
    | (Var v) :: rest -> loop (v :: acc) rest
    | list -> acc, list in
  loop [] list

let find_var key content =
  List.filter_map (function
      | Var v -> Some v
      | _ -> None) content
  |> List.assoc_opt key

let find_tspans content =
  List.filter_map (function
      | Tspan t -> Some t
      | _ -> print_endline "Warning: non-tspan object in text.";
        None) content

let map_tspan f (t : tspan) =
  let r : rect = f (rect_of_pos t.x t.y) in
  { t with x = r.x; y = r.y }

(* Apply a transform fn on the object *)
let rec map_obj f = function
  | Rect r -> Rect (f r)
  | Image (r, href) -> Image (f r, href)
  | Text t ->
    let texts = List.map (map_tspan f) t.texts in
    Text ({ t with texts })
  | Group (r, olist, []) ->
    let olist = List.map (map_obj f) olist in
    Group (f r, olist, [])
  | Group (r, _, _) as g ->
    print_endline ("Error: transforms were not applied on group: " ^ r.id);
    g

let transform_fn : transform -> rect -> rect = function
  | Translate (dx, dy) -> fun r ->
    let x, y = (r.x +. dx, r.y +. dy) in
    { r with x; y }
  | _ -> print_endline "Only translate transforms are implemented.";
    fun r -> r

(* Second (local) pass: we use the Var to fill in missing fields *)
let finish_element ?(simplify=true) data content el =
  let title = find_var "title" content |> Option.map String.trim in
  let desc = find_var "desc" content |> Option.map String.trim in
  match el with
  | Ignore -> Ignore
  | Var (key, value) ->
    if value <> "" then assert false
    else begin
      print_endline (sprintf "%s = %s" key data);
      Var (key, data)
    end
  | Obj (Rect r) -> Obj (Rect {r with title; desc})
  | Obj (Image (r, href)) -> Obj (Image ({r with title; desc}, href))
  | Obj (Text t) ->
    (* We assume that Inkscape will write the actual text in a tspan object, not
       in the text object. *)
    assert (data = "");
    let texts = find_tspans content in
    Obj (Text { t with texts })
  | Obj (Group (r, olist, tlist)) ->
    (* TODO récupérer le rect ? *)
    assert (olist = []);
    let r = { r with title; desc } in
    let olist = List.filter_map (function
        | Obj o -> Some o
        | _ -> None) content in
    let o = List.fold_left (fun o tr ->
        let f = transform_fn tr in
        map_obj f o) (Group (r, olist, [])) tlist in
    if simplify then match o with
      | Group (_, [], _) -> Ignore
      | Group (_, [o], _)-> Obj o
      | _ -> Obj o
    else Obj o
  | Tspan t -> Tspan { t with text = data }
  | Svg (canvas, olist, version) ->
    let () =  match version with
      | None -> print_endline
                  "Warning: Inkscape version not detected in SVG document"
      | Some (a,b,c) ->
        if not (List.mem (a,b,c) tested_versions)
        then print_endline
            (sprintf "Warning: Inkscape version %u.%u.%u was not \
                      tested with this version of Bogue-inkscape." a b c ) in
    assert (olist = []);
    let viewport = { canvas.viewport with title; desc } in
    let olist = List.filter_map (function
        | Obj o -> Some o
        | _ -> None) content in
    Svg ({ canvas with viewport }, olist, version)
  | Connection c -> Connection { c with action = title }

(*
Each inkscape object can contain a list of other inkscape/XML objects. Hence for
each object, we return the list of sub-objects. Among these subobjects one can
find "variables" like Title, which we use to fill-in the missing fields.
*)
let rec pull_list name data content cnxs i d : (inkscape list * string) =
  print_endline (sprintf "pull_list for [%s], d=%i" name d);
  match Xmlm.input i with
  | `El_start ((_, local), attributes) ->
    let el = parse_element attributes local in
    let el = pull_element local el cnxs i d in
    if d > 0 then pull_list name data (el :: content) cnxs i d
    else (el :: content), data
  | `El_end ->
    print_endline (sprintf "end list for [%s] (%i elements), d=%i"
                     name (List.length content) d);
    content, data
  | `Data v -> print_endline (sprintf "Data = %s" v);
    if data <> "" && v <> data
    then print_endline
        (sprintf "Warning: overwriting old data [%s] with [%s]" data v);
    pull_list name v content cnxs i d
  | `Dtd _ -> assert false

and pull_element local el cnxs i d : inkscape =
  print_endline (sprintf "pull_element [%s]" local);
  let content, data = pull_list local "" [] cnxs i (d+1) in
  match finish_element data content el with
  | Connection c -> cnxs := c :: !cnxs; Ignore
  | fel -> fel

let parse file =
  let inch = open_in file in
  let i = Xmlm.make_input (`Channel inch) in
  let content, cnxs = begin
    match try
        Xmlm.input i
      with Xmlm.Error _ -> failwith "This does not seem to be an XML file"
    with
    | `Dtd _ ->
      let cnxs = ref [] in
      let content, _data = pull_list "file" "" [] cnxs i 0 in
      content, !cnxs
    | _ -> failwith "This does not seem to be a valid XML file"
  end in
  close_in inch;
  match content with
  | [Svg s] -> s, cnxs
  | (Svg s) :: _rest -> print_endline "Selecting only the first SVG object";
    s, cnxs
  | _ -> failwith "Cannot find SVG object in file"
