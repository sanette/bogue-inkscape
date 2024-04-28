let () =
  Sys.chdir "/home/san/prog/ocaml/bogue-inkscape";
  let file = "bogue-inkscape.svg" in
  let l, c = Bogue_inkscape.import file in
  assert (List.length c = 3);
  Bogue_inkscape.to_string ~file (l, c) |> print_endline
