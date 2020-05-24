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

let neighbors g n =
  match Hashtbl.find g n with
  | Some s -> s
  | None -> Hash_set.create (module Temp)

let mcs g = 
  let v = Hash_set.create (module Temp) in
  let () = Hashtbl.iter_keys g ~f:(fun k -> Hash_set.add v k) in

  let lambda = Hashtbl.create (module Temp) in
  let () = Hash_set.iter v ~f:(fun k -> Hashtbl.set lambda ~key:k ~data:0) in

  let reverse_lambda = Hashtbl.create (module Int) in
  let () = Hashtbl.set reverse_lambda ~key:0 ~data:(Hash_set.copy v) in

  let rec mcs' acc = 
    if Hash_set.length v = 0 then List.rev acc
    else 
      let select_node () = 
        Hashtbl.fold lambda ~init:(Temp.base) 
          ~f:(fun ~key:k ~data:v k' ->
              if phys_equal k' Temp.base then k
              else
                let v' = Hashtbl.find_exn lambda k' in
                if v > v' then k else k'
            ) 
      in
      let update_lambda node =
        let nghbrs = Hash_set.inter v (neighbors g node) in
        Hash_set.iter nghbrs ~f:(
          fun nghbr -> 
            let nghbr_lm = Hashtbl.find_exn lambda nghbr in
            Hashtbl.set lambda ~key:nghbr ~data:(nghbr_lm + 1);
        )
      in

      let node = select_node () in
      update_lambda node;
      Hash_set.remove v node;
      Hashtbl.remove lambda node;
      mcs' (node::acc)
  in
  mcs' []

let string_of_order t = 
  String.concat ?sep:(Some "; ") (List.map t ~f:(Temp.string_of_temp))

let color g nodes = 
  let result = Hashtbl.create (module Temp) in
  let () = Hashtbl.iter_keys g ~f:(fun k -> Hashtbl.set result ~key:k ~data:None) in
  let rec color' g nodes =
    match nodes with
    | [] -> ()
    | n :: ns -> 
      let color_node () = 
        let nghbrs = neighbors g n in
        let colors = Hash_set.create (module Int) in
        let () = Hash_set.iter nghbrs 
            ~f:(fun nghbr ->
                let color_opt = Hashtbl.find_exn result nghbr in
                match color_opt with | Some c -> Hash_set.add colors c | _ -> ()
              )
        in
        let rec get_lowest_color c =
          let opt = Hash_set.min_elt colors ~compare:((Int.compare)) in
          match opt with
          | Some lowest_neighbor_color ->
            if c < lowest_neighbor_color then c
            else 
              let () = Hash_set.remove colors lowest_neighbor_color in 
              get_lowest_color (c+1)
          | None -> c
        in
        Hashtbl.set result ~key:n ~data:(Some (get_lowest_color 0));
      in 
      let () = color_node () in
      color' g ns
  in
  color' g nodes;
  Hashtbl.mapi result ~f:(fun ~key:_ ~data:v -> match v with | Some i -> i | None -> assert false)

let string_of_color g = 
  String.concat ?sep:(Some "\n") (Hashtbl.data (Hashtbl.mapi g ~f:(fun ~key:k ~data:v -> Temp.string_of_temp k ^ ":" ^ string_of_int v)))