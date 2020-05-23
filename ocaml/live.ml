open Core

module IR2 = Ir2

type t = (Temp.t, Temp.t Hash_set.t) Hashtbl.t

let scan instrs = 
  let seen = Hash_set.create (module Temp) in
  let result = Hashtbl.create (module Temp) in
  let add_to_graph () =
    Hash_set.iter seen ~f:(
      function x -> Hash_set.iter seen ~f:(
          function y ->
            if phys_equal x y then ()
            else
              let adj_option = Hashtbl.find result x in
              let () = match adj_option with
                | Some adj -> Hash_set.add adj y
                | None -> 
                  let adj = Hash_set.create (module Temp) in
                  let () = Hash_set.add adj y in
                  let () = Hashtbl.set result ~key:x ~data:adj in
                  ();
              in
              let adj_option = Hashtbl.find result y in
              let () = match adj_option with
                | Some adj -> Hash_set.add adj x
                | None -> 
                  let adj = Hash_set.create (module Temp) in
                  let () = Hash_set.add adj x in
                  let () = Hashtbl.set result ~key:y ~data:adj in
                  ();
              in ()
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
      add_to_graph ();
      scan' is
  in
  scan' (List.rev instrs);
  result

let string_of_scan live_scan = 
  String.concat ?sep:(Some "\n") (List.map (Hashtbl.to_alist live_scan) ~f:(
      function (node, edges) -> 
        let n = Temp.string_of_temp node in
        let es = List.map (Hash_set.to_list edges) ~f:(Temp.string_of_temp) in
        n ^ ": [" ^ (String.concat ?sep:(Some"; ") es) ^ "]"
    ))