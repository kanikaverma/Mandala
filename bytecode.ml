type binst =
Lit of int (* Push a literal *)
| Drp (* Discard a value *)
| Bin of Ast.op (* Perform arithmetic on top of stack *)
| Lod of int (* Fetch global variable *)
| Str of int (* Store TOS as global variable *)
| Lfp of int (* Load frame pointer relative *)
| Sfp of int (* Store TOS frame pointer relative *)
| Jsr of int (* Push PC, jump to function *)
| Ent of int (* Push FP, FP -> SP, SP += i *)
| Rta 		 (* sets new pc and fp *)
| Rts of int (* Restore FP, SP, consume formals, push result *)
| Beq of int (* Branch relative if top-of-stack is zero *)
| Bne of int (* Branch relative if top-of-stack is non-zero *)
| Bra of int (* Branch relative *)
| Ind of int (* Loads value relative to the top of the stack *)
| Ins of int (* stores value realtive to the top of the stack *)
| Hlt 		(* Terminate *)
(* mandala specific instructions *)
| Ogr 		(* Open graph *)


type prog = {
	num_globals : int; (* Number of global variables *)
	text : binst array; (* Code for all the functions *)
}

let string_of_stmt = function
	| Ogr -> "Ogr"
	| Lit(i) -> "Lit " ^ string_of_int i
	| Drp -> "Drp"
	| Bin(Ast.Add) -> "Add"
	| Bin(Ast.Sub) -> "Sub"
	| Bin(Ast.Mult) -> "Mul"
	| Bin(Ast.Div) -> "Div"
	| Bin(Ast.Equal) -> "Eql"
	| Bin(Ast.Neq) -> "Neq"
	| Bin(Ast.Less) -> "Lt"
	| Bin(Ast.Leq) -> "Leq"
	| Bin(Ast.Greater) -> "Gt"
	| Bin(Ast.Geq) -> "Geq"
	| Lod(i) -> "Lod " ^ string_of_int i
	| Str(i) -> "Str " ^ string_of_int i
	| Lft(i) -> "Lfp " ^ string_of_int i
	| Sfp(i) -> "Sfp " ^ string_of_int i
	| Jsr(i) -> "Jsr " ^ string_of_int i
	| Ent(i) -> "Ent " ^ string_of_int i
	| Rts(i) -> "Rts " ^ string_of_int i
	| Rta 	-> "Rta "
	| Bne(i) -> "Bne " ^ string_of_int i
	| Beq(i) -> "Beq " ^ string_of_int i
	| Bra(i) -> "Bra " ^ string_of_int i
	| Ind(i) -> "Ind " ^ string_of_int i
	| Ins(i) -> "Ins " ^ string_of_int i
	| Hlt 	-> "Hlt"

let string_of_prog p =
	string_of_int p.num_globals ^ " global variables\n" ^
	let funca = Array.mapi
		(fun i s -> string_of_int i ^ " " ^ string_of_stmt s) p.text
	in String.concat "\n" (Array.to_list funca)
