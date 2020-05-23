open Core

type t = (Temp.t, Temp.t Hash_set.t) Hashtbl.t
type edges = Temp.t Hash_set.t

let create () = Hashtbl.create (module Temp)
let add_edge g n1 n2 = 
  let adj_option = Hashtbl.find g n1 in
  let () = match adj_option with
    | Some adj -> Hash_set.add adj n2
    | None -> 
      let adj = Hash_set.create (module Temp) in
      let () = Hash_set.add adj n2 in
      let () = Hashtbl.set g ~key:n1 ~data:adj in
      ();
  in
  let adj_option = Hashtbl.find g n2 in
  let () = match adj_option with
    | Some adj -> Hash_set.add adj n1
    | None -> 
      let adj = Hash_set.create (module Temp) in
      let () = Hash_set.add adj n1 in
      let () = Hashtbl.set g ~key:n2 ~data:adj in
      ();
  in ()

let string_of_graph g = 
  String.concat ?sep:(Some "\n") (List.map (Hashtbl.to_alist g) ~f:(
      function (node, edges) -> 
        let n = Temp.string_of_temp node in
        let es = List.map (Hash_set.to_list edges) ~f:(Temp.string_of_temp) in
        n ^ ": [" ^ (String.concat ?sep:(Some"; ") es) ^ "]"
    ))

let neighbors g n = Hashtbl.find g n

let mcs g = 
  let lambda = Hashtbl.create (module Temp) in
  let reverse_lambda = Hashtbl.create (module Int) in
  let visited = Hash_set.create (module Temp) in
  let temp = Hash_set.create (module Temp) in
  let () = Hashtbl.iter_keys g 
      ~f:(function x -> 
          Hashtbl.set lambda ~key:x ~data:0;
          Hash_set.add temp x;
        ) 
  in
  let () = Hashtbl.set reverse_lambda ~key:0 ~data:temp in
  let rec mcs' g i w acc = 
    if i = Hashtbl.length g then List.rev acc
    else
      let select_node () =
        let lambda_opt = Hashtbl.find reverse_lambda w in
        let () = print_endline(string_of_int w) in
        let () = Hashtbl.iteri lambda ~f:(fun ~key:t ~data:i -> print_endline(Temp.string_of_temp t ^ ":" ^ string_of_int i)) in
        match lambda_opt with
        | Some lambda_set ->
          let v_opt = Hash_set.find lambda_set ~f:(function n -> not (Hash_set.mem visited n)) in
          let node = match v_opt with | Some v -> v | _ -> assert false in
          let nghbrs_opt = neighbors g node in
          let () = match nghbrs_opt with
            | Some ns -> 
              Hash_set.iter ns 
                ~f:(function n ->
                    let lambda_opt = Hashtbl.find lambda n in
                    match lambda_opt with
                    | Some lambda_n ->
                      let lambda_n' = lambda_n + 1 in 
                      let (opt1, opt2) = 
                        Hashtbl.find reverse_lambda lambda_n, 
                        Hashtbl.find reverse_lambda lambda_n' 
                      in
                      let () = 
                        match opt1, opt2 with
                        | Some lset, Some lset' -> 
                          Hash_set.remove lset n;
                          Hash_set.add lset' n;
                        | Some lset, None -> 
                          let new_temp = Hash_set.create (module Temp) in
                          Hash_set.remove lset n;
                          Hash_set.add new_temp n;
                          Hashtbl.set reverse_lambda ~key:lambda_n' ~data:new_temp;
                        | _ -> ()
                      in 
                      Hashtbl.set lambda ~key:n ~data:lambda_n';
                      ()
                    | None -> ()
                  )
            | None -> ()
          in
          node
        | None -> assert false
      in
      let n = select_node () in 
      Hash_set.add visited n;
      mcs' g (i+1) w' (n::acc)
  in mcs' g 0 0 []

let string_of_order t = 
  String.concat ?sep:(Some "; ") (List.map t ~f:(Temp.string_of_temp))

(* let color g nodes = 
   let ((_ : t), (_ : Temp.t list)) = g, nodes in 
   Hashtbl.create (module Temp); *)