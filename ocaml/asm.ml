open Core

exception AssemblyError of string

module L = Lexer
module IR = Ir

type t = string

let string_of_asm ir =
  (* For a Lexer.op, gives the corresponding assembly instruction. *)
  let instr_of_op op = 
    match op with
    | L.Pow -> raise (AssemblyError "idk the asm instr for ^ lol")
    | L.Plus -> "addq "
    | L.Minus -> "subq "
    | L.Times -> "imulq "
    (* TODO: idivq needs %rdx to be 0 and will clobber %rdx in the process. *)
    | L.Divide -> "idivq "
  in
  (* Calculates the stack offset for a temporary. *)
  let offset temp = -8 * (Temp.int_of_temp temp) in
  let asm_of_temp temp = string_of_int (offset temp) ^ "(%rbp)" in
  (* For an IR operand, gives the corresponding assembly value. *)
  let asm_of_operand op =
    match op with
    | IR.Immediate num -> "$" ^ (string_of_int num)
    | IR.Temporary temp -> asm_of_temp temp
  in
  (* For a given IR instruction, returns the string form. *)
  let string_of_instr instr = 
    (match instr with
     | IR.Store (temp, op) -> 
       let operand_str = asm_of_operand op in
       let instr1 = "movq " ^ operand_str ^ ", %rax" in
       let instr2 = "movq %rax, " ^ (asm_of_temp temp) in
       instr1 ^ "\n" ^ instr2
     | IR.BinOp (temp, optr, op1, op2) ->
       let optr_str = instr_of_op optr in 
       let instr1 = "movq " ^ asm_of_operand op1 ^ ", %rax" in
       let instr2 = optr_str ^ asm_of_operand op2 ^ ", %rax" in
       let instr3 = "movq %rax, " ^ asm_of_temp temp in
       instr1 ^ "\n" ^ instr2 ^ "\n" ^ instr3)
  in
  let rec string_of_asm' ir acc =
    match ir with
    | [] -> acc
    | i :: is -> string_of_asm' is (List.append acc [string_of_instr i])
  in
  let prefix =
    ".global _test_fn\n_test_fn:\npushq %rbp\nmovq %rsp, %rbp\nsubq $4096, %rsp\n"
  in
  let suffix =
    "\naddq $4096, %rsp\npopq %rbp\nretq\n"
  in
  prefix ^ (String.concat ?sep:(Some "\n") (string_of_asm' ir [])) ^ suffix

let asm_to_file asm = Out_channel.with_file "out.s" ~f:(fun out -> Out_channel.output_string out asm);
