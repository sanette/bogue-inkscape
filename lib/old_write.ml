(* Write Bogue code from simplified types *)

module C = Convert_inkscape
module I = Parse_inkscape
open Printf

let new_conn_id, init_conn_id =
  let id = ref 0 in
  (fun () -> incr id; !id),
  (fun () -> id := 0)

let no_action = "nop"
let no_trigger = "[(* write the required triggers here *)]"

let get_action c =
 Option.value ~default:no_action c.I.action

let write_connection c =
  let cid = new_conn_id () in
  let action = get_action c in
  sprintf "let c%u = W.connect %s %s %s %s in"
    cid c.I.src c.I.dst action no_trigger

let write_box r =
  sprintf "W.box ~w:%u ~h:%u ()" r.C.w r.C.h

let write_button label =
  sprintf {|W.button "%s"|} label (* TODO *)

let write_image r file =
  sprintf {|W.image ~w:%u ~h:%u "%s"|} r.C.w r.C.h file

let write_widget (w : C.widget) =
  let widget = match w.C.content with
    | Box r -> write_box r
    | Button (_r, label) -> write_button label
    | Image (r, href) -> write_image r href
  in
  sprintf "let %s = %s in" w.C.id widget

let widget_pos (w : C.widget) = (* TODO à changer ? dans C aussi *)
  match w.C.content with
  | Box r -> r.C.x, r.C.y
  | Button (r, _) -> r.C.x, r.C.y
  | Image (r, _) -> r.C.x, r.C.y

(* TODO use canvas scale? *)
let line_width x =
  if x <= 0 then 0 else if x <= 1 then 1 else x

let write_border radius w =
  let radius = match radius with
    | None -> ""
    | Some r -> sprintf "~radius:%i " r in
  sprintf "mk_border %s(mk_line ~color ~width:%u ())" radius w

let write_bg style =
  match style.C.fill, style.C.stroke_color, style.C.stroke_width with
  | None, _, None
  | None, None, _ -> None
  | Some (r,g,b,a), None, _
  | Some (r,g,b,a), _, None -> begin
      match style.C.radius with
      | None ->
        if a = 255 then Some (sprintf "L.opaque_bg (%u,%u,%u)" r g b)
        else Some (sprintf "L.color_bg (%u,%u,%u,%u)" r g b a)
      | Some radius ->
        if a <> 255 && radius <> 0
        then print_endline "Warning: currently, a border with positive [radius] \
                            is not compatible with a transparent background.";
        Some (sprintf "\n  let border = Style.(mk_border ~radius:%i (mk_line ~width:0 ())) in
  let background = Style.color_bg (%u,%u,%u,%u) in
  L.style_bg (Style.create ~background ~border ())" radius r g b a)
    end
  | None, Some (r,g,b,a), Some stroke_width ->
    let w = line_width stroke_width in
    if a <> 255 &&
       (match style.C.radius with Some r when r <> 0 -> true | _ -> false)
    then print_endline "Warning: currently, a border with positive [radius] is \
                        not compatible with a transparent background.";
    let border = write_border style.C.radius w in
    Some (sprintf "let color = (%u,%u,%u,%u) in\n  L.style_bg Style.(of_border (%s))" r g b a border)
  | Some (r,g,b,a), Some (lr,lg,lb,la), Some stroke_width ->
    let w = line_width stroke_width in
    let border = write_border style.C.radius w in
    Some (sprintf "let color = (%u,%u,%u,%u) in
  let border = Style.(%s) in
  let background = Style.color_bg (%u,%u,%u,%u) in
  L.style_bg (Style.create ~background ~border ())" lr lg lb la border r g b a)

let write_resident name r style (w : C.widget) =
  let name = match name with
    | None -> ""
    | Some t -> sprintf {| ~name:"%s"|} t in
  let bg, opt = match write_bg style with
    | None -> "", ""
    | Some s -> "let background = " ^ s ^ " in\n", "~background " in
  sprintf "%s  L.resident%s %s~x:%i ~y:%i ~w:%u ~h:%u %s"
    bg name opt r.C.x r.C.y r.C.w r.C.h w.C.id

(* TODO don't use superpose when there is only 1 element *)
let write_rooms name r list =
  let name = match name with
    | None -> ""
    | Some t -> sprintf {| ~name:"%s"|} t in
  let ids = List.map (fun (w : C.layout) -> w.C.id) list in
  let w = if r.C.w = 0 then "" else sprintf " ~w:%u" r.C.w in
  let h = if r.C.h = 0 then "" else sprintf " ~h:%u" r.C.h in
  sprintf "L.superpose%s%s%s [%s]" name w h (String.concat "; " ids)

let write_layout (room : C.layout) =
  let layout = match room.C.content with
    | Resident w -> write_resident room.C.name room.C.rect room.style w
    | Rooms list -> write_rooms room.C.name room.C.rect list in
  sprintf "let %s = %s in" room.C.id layout

let write_action name =
  sprintf {|let %s _src _dst _ev = print_endline "%s" in|} name name

let get_all_actions cs =
  let actions = ref C.SSet.empty in
  cs |> List.iter (fun c ->
      let action = get_action c in
      actions := C.SSet.add action !actions);
  C.SSet.elements !actions

let write layout connections =
  let buffer = Buffer.create 16 in
  let add s = Buffer.add_string buffer (s ^ "\n") in
  add "open Bogue";
  add "module L = Layout";
  add "module W = Widget";
  add "\nlet main () =\n";
  add "(* Complete the action functions below: *)";
  get_all_actions connections
  |> List.iter (fun a -> add (write_action a));
  add "";
  let rec loop (l : C.layout) =
    let () = match l.C.content with
      | Resident w -> write_widget w |> add
      | Rooms list -> List.iter loop list in
    write_layout l |> add in
  loop layout;
  init_conn_id ();
  add "";
  List.iter (fun c -> add (write_connection c)) connections;
  let cs = Array.init (new_conn_id  () - 1) (fun i -> "c" ^ (string_of_int (i+1)))
           |> Array.to_list
           |> String.concat "; " in
  add (sprintf "let connections = [%s] in" cs);
  add (sprintf "\nBogue.(run (of_layout ~connections %s))" layout.id);
  Buffer.add_string buffer "\nlet () = main ()\n";
  Buffer.contents buffer


(*
dune utop
Sys.chdir "/home/san/prog/ocaml/bogue-inkscape";;
let svg, c = Bogue_inkscape__Parse_inkscape.parse "bogue-inkscape.svg";;
let l, c = Bogue_inkscape__Convert_inkscape.convert svg c;;
Bogue_inkscape__Write_bogue.write l c |> print_endline;;

let rect5 = W.box ~w:908 ~h:41 () in
let rect5_l = L.resident ~x:1 ~y:48 ~w:908 ~h:41 rect5 in
let rect3 = W.box ~w:182 ~h:86 () in
let rect3_l = L.resident ~x:612 ~y:236 ~w:182 ~h:86 rect3 in
let rect2 = W.box ~w:185 ~h:77 () in
let rect2_l = L.resident ~x:626 ~y:132 ~w:185 ~h:77 rect2 in
let g5 = L.superpose ~w:0 ~h:0 [rect3_l; rect2_l] in
let button = W.box ~w:701 ~h:110 () in
let button_l = L.resident ~x:63 ~y:974 ~w:701 ~h:110 button in
let button2 = W.box ~w:450 ~h:277 () in
let button2_l = L.resident ~x:252 ~y:536 ~w:450 ~h:277 button2 in
let rect1 = W.box ~w:461 ~h:284 () in
let rect1_l = L.resident ~x:60 ~y:135 ~w:461 ~h:284 rect1 in
let layer1 = L.superpose ~w:0 ~h:0 [rect5_l; g5; button_l; button2_l; rect1_l] in
let svg1 = L.superpose ~w:909 ~h:1286 [layer1] in
let c1 = W.connect rect1_l rect3_l nop TODO in
let c2 = W.connect button rect1_l mon_action TODO in
let c3 = W.connect button rect1_l nop TODO in
Bogue.(run (of_layout svg1))



*)
