(* This file is part of Bogue-inkscape

   Copyright Vu Ngoc San 2024

   This module is used to sanitize Inkscape ids to make sure they are valid
   ocaml identifiers.
*)

module SSet = Set.Make (String)
open Printf

let is_letter char =
  let c = Char.code char in
  (c <= Char.code 'z' && c >= Char.code 'a')
  || (c <= Char.code 'Z' && c >= Char.code 'A')

let is_digit char =
  let c = Char.code char in
  c <= Char.code '9' && c >= Char.code '0'

(* One could use `ubase` for better results. *)
let sanitize_id s =
  let message = ref [] in
  let add msg = message := msg :: !message in
  if s = "" then
    raise (Invalid_argument "Identifier String should not be empty");
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if is_digit c || is_letter c || c = '_' then Buffer.add_char b c
      else if c = ' ' then (
        add "Space char was replaced by underscore";
        Buffer.add_char b '_')
      else add (sprintf "Skipping invalid char : [%c]" c))
    s;
  let res = Buffer.contents b in
  if res = "" then
    raise
      (Invalid_argument
         (sprintf "Identifier String [%s] contains only invalid chars" res));
  let c = res.[0] in
  let res =
    if Char.lowercase_ascii c = c then res
    else (
      add (sprintf "Lowering the case of the first letter of [%s]" res);
      String.make 1 (Char.lowercase_ascii c)
      ^ String.sub res 1 (String.length res - 1))
  in
  if res = s then Ok res
  else (
    add (sprintf "Identifier [%s] was sanitized to [%s]" s res);
    Error (res, List.rev !message))

let test_sanitize () =
  let ok s = failwith (sprintf "[%s] should not be OK" s) in
  let error = fst in
  assert (sanitize_id "good" = Ok "good");
  assert (sanitize_id "ABCD" |> Result.fold ~ok ~error = "aBCD");
  assert (sanitize_id "c'est nul" |> Result.fold ~ok ~error = "cest_nul");
  assert (sanitize_id "çA Vient" |> Result.fold ~ok ~error = "a_Vient");
  assert (sanitize_id "_B_C" = Ok "_B_C")

(* From a set of ids, return a table (id, sanitized_id) where sanitized_id are
   all different. *)
let sanitize_set pool =
  let pool2 = ref SSet.empty in
  let safe = Hashtbl.create 20 in
  SSet.iter
    (fun id ->
      match sanitize_id id with
      | Ok sid -> Hashtbl.add safe id sid
      | Error (sid, msg) ->
          List.iter print_endline msg;
          let rec loop s i =
            if SSet.mem s pool || SSet.mem s !pool2 then
              loop (sid ^ string_of_int (i + 1)) (i + 1)
            else s
          in
          let sid = loop sid 1 in
          pool2 := SSet.add sid !pool2;
          Hashtbl.add safe id sid)
    pool;
  (safe, !pool2)

let print_table t =
  Hashtbl.iter (fun id sid -> print_endline (sprintf "[%s] ==> [%s]" id sid)) t

(*

#  SSet.of_list (["Abcd"; "x"; "y"; "z"; "abcd"; "aBcd"; "Abcd2"; "a,bcd"; "éabcd"; "abcd2"])
|> sanitize_set
|> fst
|> print_table;;
Lowering the case of the first letter of [Abcd]
Identifier [Abcd] was sanitized to [abcd]
Lowering the case of the first letter of [Abcd2]
Identifier [Abcd2] was sanitized to [abcd2]
Skipping invalid char : [,]
Identifier [a,bcd] was sanitized to [abcd]
Skipping invalid char : [�]
Skipping invalid char : [�]
Identifier [éabcd] was sanitized to [abcd]
[z] ==> [z]
[éabcd] ==> [abcd5]
[Abcd2] ==> [abcd22]
[abcd] ==> [abcd]
[aBcd] ==> [aBcd]
[a,bcd] ==> [abcd4]
[y] ==> [y]
[x] ==> [x]
[abcd2] ==> [abcd2]
[Abcd] ==> [abcd3]
- : unit = ()

*)

(* Check if [sfx] is a good suffix that one can append to the ids in pool to
   make them distinguishable from non-suffixed ids. Otherwise modify the suffix
   (by adding a number) to make it so. *)
let valid_suffix sfx pool =
  let rec loop suffix i =
    if SSet.exists (String.ends_with ~suffix) pool then
      loop (suffix ^ string_of_int (i + 1)) (i + 1)
    else (suffix, i)
  in
  let suffix, i = loop sfx 1 in
  if i = 1 then Ok suffix
  else
    Error
      ( suffix,
        sprintf
          "Suffix [%s] had to be modified to [%s] because some ids already end \
           with [%s]"
          sfx suffix sfx )

(*

 SSet.of_list ["abc_l"; "x"; "y"; "z"]
    |> valid_suffix "_l";;
- : (string, string * string) result =
Error
 ("_l2",
  "Suffix [_l] had to be modified to [_l2] because some ids already end with [_l]")
*)
