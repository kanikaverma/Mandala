open Ast
open Bytecode

module StringMap = Map.Make(String)

(* Environment: symbol tables for functions, global and local vars *)
type env = {
	function_index  : int StringMap.t; 	(* Index for each function *)
	global_index    : int StringMap.t; 	(* Address for global vars *)
	local_index     : int StringMap.t; 	(* FP offset for args, locals *)
}

(* enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
	  [] -> []
	| hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs = 
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs 

let translate (globals, functions) = 
  let global_indexes = string_map_pairs StringMap.empty (enum 1 0 globals) in 
  let built_in_functions = StringMap.add "print" (-1) StringMap.empty in
  let function_indexes = string_map_pairs built_in_functions 
    (enum 1 1 (List.map (fun f -> f.fname) functions)) in 
  let translate env fdecl = 
    let num_formals = List.length fdecl.formals
    and num_locals = List.length fdecl.locals
    and local_offsets = enum 1 1 fdecl.locals
    and formal_offsets = enum (-1) (-2) fdecl.formals in 
    let env = { env with local_index = string_map_pairs
                StringMap.empty (local_offsets @ formal_offsets) } in 
    let rec expr = function
        Literal i -> [Lit i]
      | Id s ->
          (try [Lfp (StringMap.find s env.local_index)]
           with Not_found -> try [Lod (StringMap.find s env.global_index)]
           with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Binop (e1, op, e2) -> expr e1 @ expr e2 @ [Bin op]
      | Assign (s, e) -> expr e @ 
          (try [Sfp (StringMap.find s env.local_index)]
           with Not_found -> try [Str (StringMap.find s env.global_index)]
           with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Call (fname, actuals) -> (try 
          (List.concat (List.map expr (List.rev actuals))) @ 
          [Jsr (StringMap.find fname env.function_index) ]
        with Not_found -> raise (Failure ("undefined function " ^ fname)))
      | Noexpr -> [] 
    in 
    let rec stmt = function
        Block s1       -> List.concat (List.map stmt s1) 
      | Expr e         -> expr e @ [Drp]
      | Return e       -> expr e @ [Rts num_formals]
      | IF (p, t, f)   -> let t' = stmt t and f' = stmt f in 
        expr p @ [Beq(2 + List.length t')] @
        t' @ [Bra(1 + List.length f')] @ f'
      (* | Foreach -> -> -> need this *)

    in [Ent num_locals] @
    stmt (Block fdecl.body) @
    [Lit 0; Rts num_formals]

  in 
  let env = { function_index = function_indexes;
              global_index = global_indexes;
              local_index = StringMap.empty } in 

  let entry_function = try 
    [Jsr (StringMap.find "main" function_indexes); Hlt]
  while Not_found -> raise (Failure ("no \"main\" function"))
  in
  let func_bodies = entry_function :: List.map (translate env) functions in 
  let (fun_offset_list, _) = List.fold_left
      (fun (1, i) f -> (i :: l, (i + List.length f))) ([], 0) func_bodies in 
  let func_offset = Array.of_list (List.rev fun_offset_list) in 
  { num_globals = List.length globals; 
    text = Array.of_list (List.map (function
        Jsr i when i > 0 -> Jsr func_offset.(i)
      | _ as s -> s) (List.concat func_bodies))
  }