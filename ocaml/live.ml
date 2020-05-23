open Core

module IR2 = Ir2

let scan instrs = 
  let seen = Hash_set.create (module Temp) in
  (* TODO: for debugging. *)
  (* let string_of_seen () = 
     "[" ^ String.concat ?sep:(Some "; ") (List.map (Hash_set.to_list seen) ~f:Temp.string_of_temp) ^ "]"
     in *)
  let result = Graph.create () in
  let add_to_graph () =
    Hash_set.iter seen ~f:(
      function x -> Hash_set.iter seen ~f:(
          function y ->
            if phys_equal x y then ()
            else Graph.add_edge result x y
        )
    )
  in
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
  let rec scan' instrs = 
    let temp_from_op op = 
      match op with
      | IR2.Immediate _ -> None
      | IR2.Temporary temp -> Some temp
    in
    match instrs with
    | [] -> ()
    | i :: is -> 
      let (uses, defs) = 
        match i with 
        | IR2.Store (temp, op) -> ([temp_from_op op], [Some temp])
        | IR2.BinOp (_, temp, op) -> ([Some temp; temp_from_op op], [])
      in
      remove_all defs;
      add_all uses;
      (* print_endline(string_of_seen ()); *)
      add_to_graph ();
      scan' is
  in
  scan' (List.rev instrs);
  result