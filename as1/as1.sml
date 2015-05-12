(* CPSC 421 Compilers and Interpreters Spring 2015*)
(* Student: Lien Tran BR 2016 *)
(* Assignment #1 due January 28th 2015 *)

type id = string

datatype binop = PLUS | MINUS | TIMES | DIV

datatype stm = SEQ of stm * stm
	     | ASSIGN of id * exp
	     | PRINT of exp list

     and exp = VAR of id
	     | CONST of int
             | BINOP of exp * binop * exp
             | ESEQ of stm * exp

(* return greater int, used to compare integers 
and return the greater one for maxargs *)
fun max(x,y) = 
	if x >= y then x
	else y;

(* PART 1 *)
(* multually exclusive functions to find max # PRINT args *)

(* first function maxargs deals with statements.
	Specifically, recursively calling on maxargs,
	maxexp or maxlst depending on the pattern match. *)
fun 
	maxargs(SEQ(x1,x2)) = max(maxargs(x1), maxargs(x2))
|	maxargs(ASSIGN(x1, x2)) = maxexp(x2)
|	maxargs(PRINT(lst)) = maxlst(lst, length(lst))
and 
(* maxexp deals with evaluating expressions, usually
	returning 0 or results from recursive calls to
	maxargs or maxexp that may imply PRINT statements
	that are nested deeper in the expressions. *)
	maxexp(VAR(x)) = 0
|	maxexp(CONST(x)) = 0
| 	maxexp(BINOP(x1,x2,x3)) = max(maxexp(x1), maxexp(x3))
|	maxexp(ESEQ(x1, x2)) = max(maxargs(x1), maxexp(x2))
and
(* maxlst is only called when a PRINT statement is 
	encountered. msf denotes "max so far". The 
	function starts off with msf as the length of the 
	PRINT list, and changes that if there are PRINT
	statements deeper within the list with more arguments. *)
	maxlst(nil, msf) = msf
|	maxlst(l::ls, msf) = maxlst(ls, max(maxexp(l), msf));   


(* PART 2 *)
(* an interpreter for a straight-line programming language*)

(* Tables in this interpreter are treated as lists.
	Update operations simply add the new key-value
	pair on top of the table. This update function
	acts as a helper to interpStm and interpExp. *)
fun update(s:id, i:int, table) = (s,i)::table;


(* Lookup operation is a helper function, as well.
	Returns ~1 when key doesn't exist. *)
fun lookup(nil, key:id) = ~1
|	lookup((s,i)::table, key:id) = 
		if s = key then i
		else lookup(table, key);


(* Simple helper function to deal with BINOP
	operations in the straight-line programming
	language. Simply carries out the operations
	as specfified. *)
fun binop(op1, PLUS, op2) = op1 + op2
|	binop(op1, MINUS, op2) = op1 - op2
|	binop(op1, TIMES, op2) = op1 * op2
|	binop(op1, DIV, op2) = op1 div op2;

(* interpStm and interpExp are mutually exclusive as 
	specified in the assignment, with the addition of 
	interpPrtLst as a helper to deal with PRINT
	statements. *)
(* interpStm deals with stms, and updates the table
	as it processes these stms from left to right. 
	Exclusively calls the update operation to handle
	ASSIGN statements. *)
fun 
	interpStm(SEQ(stm1,stm2), table) = interpStm(stm2, interpStm(stm1, table))
|	interpStm(ASSIGN(s,e), table) = 
		let 
			val (i, newtable) = interpExp(e,table)
		in
			update(s, i, newtable)
		end
| 	interpStm(PRINT(lst),table) = interpPrtLst(lst,table)
and 
(* interpExp is similar to interpStm, but deals with exps
	and therefore calls on binop and lookup operations. *)
	interpExp(VAR(s),table) = (lookup(table, s), table)
|	interpExp(CONST(i),table) = (i,table)
| 	interpExp(BINOP(e1,b,e2),table) =
		let
			val (i1, newtable) = interpExp(e1,table)
		in
			let
				val (i2, newertable) = interpExp(e2,newtable)
			in
				(binop(i1, b, i2), newertable)
			end
		end
| 	interpExp(ESEQ(st,e),table) = interpExp(e,interpStm(st,table))
and 
(* interpPrtLst deals with PRINT statements exclusively. It 
	recursively prints values in its expressions and 
	calls on interpExp to do so. The end of each PRINT
	statement is followed by a newline. *)
	interpPrtLst(nil,table) = (print "\n"; table)
|	interpPrtLst(l::ls,table) = 
		let
			val (i, newtable) = interpExp(l,table)
		in
			(print(Int.fmt StringCvt.DEC i ^ " "); interpPrtLst(ls, newtable))
		end;


(* wrapper function interp that calls on interpStm,
	return unit. *)
fun interp(st) = (interpStm(st,nil); ());
	
