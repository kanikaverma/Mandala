open Ast
open Bytecode

module StringMap = Map.Make(String)

(* Environemnt: symbol tables for functions, global and local vars *)
type env = {
	function_index 		: int StringMap.t; 	(* Index for each function *)
	global_index 		: int StringMap.t; 	(* Address for global vars *)
	local_index 		: int StringMap.t; 	(* FB offset for args, locals *)
}