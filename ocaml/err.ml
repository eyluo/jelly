open Core

let print file fname mark =
  let file_list = String.split file ~on:'\n' in
  let repeat s n = List.init n ~f:(fun _ -> s) |> String.concat in
  let r1, c1 = Mark.start mark in
  let r2, c2 = Mark.stop mark in
  let line =
    match List.nth file_list (r1 - 1) with
    | Some str -> str
    | None -> ""
  in
  let underline =
    if r1 = r2
    then repeat " " (c1 - 1) ^ repeat "^" (c2 - c1 + 1)
    else repeat " " (c1 - 1) ^ "^"
  in
  Printf.printf "%s: %d:%d-%d:%d\n%s\n%s\n" fname r1 c1 r2 c2 line underline
;;
