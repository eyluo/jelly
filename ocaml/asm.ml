open Core

exception AssemblyError of string

module L = Lexer
module IR3 = Ir3

type t = string

let to_string ir =
  (* Calculates the stack offset for a temporary. *)
  let offset temp = -8 * Temp.int_of_temp temp in
  let asm_of_temp temp = string_of_int (offset temp) ^ "(%rbp)" in
  (* For an IR operand, gives the corresponding assembly value. *)
  let asm_of_operand op =
    match op with
    | IR3.Immediate num -> "$" ^ string_of_int num
    | IR3.Temporary temp -> asm_of_temp temp
  in
  (* For a Lexer.op, gives the corresponding assembly instruction. *)
  let instr_of_op op operand =
    let opnd = asm_of_operand operand in
    let result =
      match op with
      | L.Op.Pow -> raise (AssemblyError "idk the asm instr for ^ lol")
      | L.Op.Plus -> "addq " ^ opnd ^ ", %rax"
      | L.Op.Minus -> "subq " ^ opnd ^ ", %rax"
      | L.Op.Times -> "imulq " ^ opnd ^ ", %rax"
      (* TODO: idivq needs to sign-extend into %rdx (look into the cqto instr): 
       * idivq val -> divide %rdx:%rax by val . *)
      | L.Op.Divide -> "movq " ^ opnd ^ ", %rdx\ncqto\nidivq " ^ opnd
      | _ -> raise (AssemblyError "mapping not present yet")
    in
    result
  in
  (* For a given IR instruction, returns the string form. *)
  let string_of_instr instr =
    match instr with
    | IR3.Store (temp, op) ->
      let operand_str = asm_of_operand op in
      let instr1 = "movq " ^ operand_str ^ ", %rax" in
      let instr2 = "movq %rax, " ^ asm_of_temp temp in
      instr1 ^ "\n" ^ instr2
    | IR3.BinOp (temp, optr, op1, op2) ->
      let instr1 = "movq " ^ asm_of_operand op1 ^ ", %rax" in
      let instr2 = instr_of_op optr op2 in
      let instr3 = "movq %rax, " ^ asm_of_temp temp in
      instr1 ^ "\n" ^ instr2 ^ "\n" ^ instr3
  in
  let rec to_string' ir acc =
    match ir with
    | [] -> acc
    | i :: is -> to_string' is (List.append acc [ string_of_instr i ])
  in
  let prefix =
    ".global _test_fn\n_test_fn:\npushq %rbp\nmovq %rsp, %rbp\nsubq $4096, %rsp\n"
  in
  let suffix = "\naddq $4096, %rsp\npopq %rbp\nretq\n" in
  prefix ^ String.concat ?sep:(Some "\n") (to_string' ir []) ^ suffix
;;

let asm_to_file outpath asm =
  Out_channel.with_file outpath ~f:(fun out -> Out_channel.output_string out asm)
;;
