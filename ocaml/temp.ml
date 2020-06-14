open Core

type t = string [@@deriving sexp, compare, hash]

(* TODO: I have forgotten how to curry functions, but I think that that might
 * be a useful way to get rid of this global. *)
let counter = ref 0
let base = ""

let reset () =
  let () = counter := 0 in
  ()
;;

let create () =
  let increment' () =
    counter := !counter + 1;
    let idx = !counter in
    "t" ^ string_of_int idx
  in
  increment' ()
;;

let to_string t = t
let int_of_temp t = Scanf.sscanf t "t%d" (fun x -> x)
