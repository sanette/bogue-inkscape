(* Write Bogue code from simplified types *)

module C = Convert_inkscape
module I = Parse_inkscape
module F = Format
open Printf

let new_conn_id, init_conn_id =
  let id = ref 0 in
  (fun () -> incr id; !id),
  (fun () -> id := 0)

let no_action = "nop"
let no_trigger = "[(* write the required triggers here *)]"

let get_action c =
 Option.value ~default:no_action c.I.action

let quote s = sprintf {|"%s"|} s

let soption label to_string o =
  match o with
  | None -> ""
  | Some value -> sprintf " ~%s:%s" label (to_string value)

let scolor (r,g,b,a) =
  sprintf "(%u,%u,%u,%u)" r g b a

let sconnection c =
  let cid = new_conn_id () in
  let action = get_action c in
  sprintf "let c%u = W.connect %s %s %s %s in"
    cid c.I.src c.I.dst action no_trigger

let write_box out r =
  F.fprintf out "@[<hov 2>W.box ~w:%u ~h:%u ()@]" r.C.w r.C.h

let write_button out style text =
  let border_radius = soption "border_radius" string_of_int style.C.radius in
  let border_color = soption "border_color" scolor style.C.stroke_color in
  F.fprintf out {|@[<hov 2>W.button%s%s "%s"@]|} border_radius border_color text (* TODO *)

let write_image out r file =
  F.fprintf out {|@[<hov 2>W.image ~w:%u ~h:%u "%s"@]|} r.C.w r.C.h file

(* https://www.w3.org/TR/SVG/text.html#GlyphsMetrics *)
let write_label out label =
  let font = soption "font" quote label.C.font in
  F.fprintf out {|@[<hov 2>W.label ~size:%u%s ~fg:%s@ "%s"@]|}
    label.C.size font (scolor label.C.color) label.C.text

let write_widget out style (w : C.widget) =
  F.fprintf out "@[<hov 2>let %s =@ " w.C.id;
  let () = match w.C.content with
    | Box r -> write_box out r
    | Button (_r, text) -> write_button out style text
    | Image (r, href) -> write_image out r href
    | Label (_r, label) -> write_label out label
  in
  F.fprintf out " in@]@;<1 0>"

let widget_pos (w : C.widget) = (* TODO à changer ? dans C aussi *)
  match w.C.content with
  | Box r -> r.C.x, r.C.y
  | Button (r, _) -> r.C.x, r.C.y
  | Image (r, _) -> r.C.x, r.C.y
  | Label (r, _) -> r.C.x, r.C.y

(* TODO use canvas scale? *)
let line_width x =
  if x <= 0 then 0 else if x <= 1 then 1 else x

let sborder radius w =
  let radius = soption "radius" string_of_int radius in
  sprintf "mk_border%s (mk_line ~color ~width:%u ())" radius w

let write_bg out style =
  match style.C.fill, style.C.stroke_color, style.C.stroke_width with
  | None, _, None
  | None, None, _ -> None
  | Some (r,g,b,a), None, _
  | Some (r,g,b,a), _, None -> begin
      match style.C.radius with
      | None ->
        if a = 255 then Some (fun () ->
            F.fprintf out "@[<hov 2>L.opaque_bg (%u,%u,%u)@]" r g b)
        else Some (fun () ->
            F.fprintf out "@[<hov 2>L.color_bg (%u,%u,%u,%u)@]" r g b a)
      | Some radius ->
        if a <> 255 && radius <> 0
        then print_endline "Warning: currently, a border with positive [radius] \
                            is not compatible with a transparent background.";
        Some (fun () ->
            F.fprintf out "@[<hov 2>let border =@ Style.(mk_border ~radius:%i (mk_line ~width:0 ())) in@]@;<1 0>@[<hov 2>let background =@ Style.color_bg (%u,%u,%u,%u) in@]@;<1 0>@[<hov 2>L.style_bg (Style.create ~background ~border ())@]" radius r g b a)
    end
  | None, Some (r,g,b,a), Some stroke_width ->
    let w = line_width stroke_width in
    if a <> 255 &&
       (match style.C.radius with Some r when r <> 0 -> true | _ -> false)
    then print_endline "Warning: currently, a border with positive [radius] is \
                        not compatible with a transparent background.";
    let border = sborder style.C.radius w in
    Some (fun () ->
        F.fprintf out "@[<hov 2>let color = %s in@]@;<1 0>@[<hov 2>L.style_bg Style.(of_border (%s))@]" (scolor (r,g,b,a)) border)
  | Some c, Some lc, Some stroke_width ->
    let w = line_width stroke_width in
    let border = sborder style.C.radius w in
    Some (fun () ->
        F.fprintf out "@[<hov 2>let color = %s in@]@;<1 0>@[<hov 2>let border =@ Style.(%s) in@]@;<1 0>@[<hov 2>let background =@ Style.color_bg %s in@]@;<1 0>@[<hov 2>L.style_bg (Style.create ~background ~border ())@]" (scolor lc) border (scolor c))

let pr_x x =
  if x >= 0 then sprintf " ~x:%u" x else sprintf " ~x:(%i)" x

let pr_y y =
  if y >= 0 then sprintf " ~y:%u" y else sprintf " ~y:(%i)" y

let write_resident out name r style (wg : C.widget) =
  let name = soption "name" quote name in
  let opt = match write_bg out style with
    | None ->  ""
    | Some f -> F.fprintf out "@[<hov 2>let background =@ ";
      f ();
      F.fprintf out " in@]@;<1 0>";
      "~background" in
  let w = if r.C.w = 0 then "" else sprintf " ~w:%u" r.C.w in
  let h = if r.C.h = 0 then "" else sprintf " ~h:%u" r.C.h in
  F.fprintf out "@[<hov 2>L.resident%s@ %s%s%s%s%s@ %s@]@;"
    name opt (pr_x r.C.x) (pr_y r.C.y) w h wg.C.id

(* TODO don't use superpose when there is only 1 element *)
let write_rooms out name r list =
  let name = soption "name" quote name in
  let ids = List.map (fun (w : C.layout) -> w.C.id) list in
  let w = if r.C.w = 0 then "" else sprintf " ~w:%u" r.C.w in
  let h = if r.C.h = 0 then "" else sprintf " ~h:%u" r.C.h in
  F.fprintf out "@[<hov 2>L.superpose%s%s%s [%s]@]@;" name w h (String.concat "; " ids)

let write_layout out (room : C.layout) =
  F.fprintf out "@[<hov 2>let %s =@ " room.C.id;
  let () = match room.C.content with
  | Resident w -> write_resident out room.C.name room.C.rect room.style w
  | Rooms list -> write_rooms out room.C.name room.C.rect list in
  F.fprintf out "in@]@;<1 0>"

let write_action out name =
  F.fprintf out
    {|@[<hov 2>let %s _src _dst _ev =@;<1 2>print_endline "%s" in@]@ |} name name

let get_all_actions cs =
  let actions = ref C.SSet.empty in
  cs |> List.iter (fun c ->
      let action = get_action c in
      actions := C.SSet.add action !actions);
  C.SSet.elements !actions

let write ?file layout connections =
  let buffer = Buffer.create 16 in
  let out = F.formatter_of_buffer buffer in
  let br () = F.fprintf out "@[<v 0>@]@;" (* blank line *) in
  let add s = let open F in
    pp_open_hovbox out 0;
    pp_print_string out s;
    pp_close_box out ();
    pp_print_break out 1 0 in
  F.pp_open_vbox out 0;
  Option.iter (fun file -> F.fprintf out "@[<v 3>(* This file is generated by Bogue_inkscape@ from the Inkscape file [%s]. *)@]@;" file; br ()) file;
  add "open Bogue";
  add "module L = Layout";
  add "module W = Widget";
  br ();
  add "let main () =";
  F.pp_open_vbox out 2;
  br ();

  let cs = get_all_actions connections in
  if cs <> [] then begin
    F.pp_open_vbox out 0;
    add "(* Complete the action functions below: *)";
    List.iter (fun a -> write_action out a) cs;
    F.pp_close_box out ();
    br ();
  end;

  let rec loop (l : C.layout) =
    let () = match l.C.content with
      | Resident w -> write_widget out l.C.style w
      | Rooms list -> List.iter loop list in
    write_layout out l in
  loop layout;
  init_conn_id ();
  br ();
  List.iter (fun c -> add (sconnection c)) connections;
  let cs = Array.init (new_conn_id  () - 1) (fun i -> "c" ^ (string_of_int (i+1)))
           |> Array.to_list
           |> String.concat "; " in
  let c_opt = match connections with
    | [] -> ""
    | _ -> add (sprintf "let connections = [%s] in" cs); br (); " ~connections" in
  add (sprintf "Bogue.(run (of_layout%s %s))" c_opt layout.id);
  F.pp_close_box out ();
  F.pp_print_flush out ();
  Buffer.add_string buffer "\nlet () = main ()\n";
  Buffer.contents buffer


(*

dune utop
Sys.chdir "/home/san/prog/ocaml/bogue-inkscape";;
let svg, c = Bogue_inkscape__Parse_inkscape.parse "bogue-inkscape.svg";;
let l, c = Bogue_inkscape__Convert_inkscape.convert svg c;;
Bogue_inkscape__Write_bogue.write l c |> print_endline;;

*)

let test () =
  let buffer = Buffer.create 16 in
  let out = F.formatter_of_buffer buffer in
  F.fprintf out "@[<v>Debut@;";
  F.fprintf out "@[<v 2>Boite v 2@;<1 0>2eme ligne@ ";
  F.fprintf out "Fin boite@]@;<1 0>";
  F.fprintf out "Fin@]";
  F.pp_print_flush out ();
  Buffer.contents buffer

(*

Debut
Boite v 2
  2eme ligne
  Fin boite
Fin


*)
