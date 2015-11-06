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
| Rts of int (* Restore FP, SP, consume formals, push result *)
| Beq of int (* Branch relative if top-of-stack is zero *)
| Bne of int (* Branch relative if top-of-stack is non-zero *)
| Bra of int (* Branch relative *)
| Hlt (* Terminate *)


type prog = {
num_globals : int; (* Number of global variables *)
text : binst array; (* Code for all the functions *)
}