(* This file is part of Bogue-inkscape

   Copyright Vu Ngoc San 2024

*)


let import file =
  let svg, c = Parse_inkscape.parse file in
  Convert_inkscape.convert svg c

let to_string ?file (l,c) =
  Write_bogue.write ?file l c

let file_to_string file =
  to_string ~file (import file)

let convert ~output file =
  let code = file_to_string file in
  Out_channel.with_open_text output (fun outch ->
      output_string outch code)

(*

Sys.chdir "/home/san/prog/ocaml/bogue-inkscape";;
Bogue_inkscape.file_to_string "bogue-inkscape-2.svg" |> print_endline;;
*)
