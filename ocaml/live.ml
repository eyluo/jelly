open Core

module IR2 = Ir2

type t = Temp.t list list

let scan instrs = 
  let seen = Hash_set.create (module Temp) in
  let rec remove_all defs =
    match defs with
    | [] -> ()
    | d :: ds -> 
      (match d with
       | Some d' -> Hash_set.remove seen d'; remove_all ds
       | _ -> remove_all ds)
  in
  let rec add_all uses = 
    match uses with
    | [] -> ()
    | u :: us -> 
      (match u with 
       | Some u' -> Hash_set.add seen u'; add_all us
       | _ -> add_all us)
  in
  let list_of () = 
    Hash_set.to_list seen
  in
  let rec scan' instrs acc = 
    let temp_from_op op = 
      match op with
      | IR2.Immediate _ -> None
      | IR2.Temporary temp -> Some temp
    in
    match instrs with
    | [] -> acc
    | i :: is -> 
      let (uses, defs) = 
        match i with 
        | IR2.Store (temp, op) -> ([temp_from_op op], [Some temp])
        | IR2.BinOp (_, temp, op) -> ([Some temp; temp_from_op op], [])
      in
      remove_all defs;
      add_all uses;
      let currently_live = list_of () in
      scan' is (currently_live::acc)
  in
  scan' (List.rev instrs) []

let string_of_scan live_scan = 
  let string_of_elem e = 
    String.concat ?sep:(Some ", ") (List.map e ~f:(Temp.string_of_temp)) 
  in
  String.concat ?sep:(Some ";\n\n") (List.map live_scan ~f:(string_of_elem))