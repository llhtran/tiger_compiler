(* frame.sml *)
(* CPSC 421 Compilers and Interpreters Spring 2015 *)
(* Student: Lien Tran BR 2016 *)

signature FRAME =
sig 
  type offset = int
  type frame
  val ws : int 
  (* val exp : *)

  val newFrame : int -> frame * offset list
  val allocInFrame : frame -> offset
  (*val addParam : frame * int -> frame*)
  val incrLocal : frame * int -> frame
  val increMax : frame -> frame 
  val updateMaxargs : frame * int -> unit

  datatype frag = PROC of {name : Temp.label, body: Tree.stm, frame: frame} 
                | DATA of {lab : Temp.label, s: string}

  val str : Temp.label * string -> frag
  val makeFrag : {name: Temp.label, body: Tree.stm, frame: frame} -> frag
  (*val printfrag : frag list -> unit*)

  val externalCall: string * Tree.exp list -> Tree.exp 

end (* signature FRAME *)


structure Frame : FRAME = 
struct

  type offset = int
  type frame = {formals : int,         (* number of formal parameters *)
                offlst : offset list,  (* offset list for formals *)
                locals : int ref,      (* # of local variables so far *)
                maxargs : int ref}     (* max outgoing args for the function *)

  val ws = 4 (* word size *)

  datatype frag = PROC of {name : Temp.label, body: Tree.stm, frame: frame} 
                | DATA of {lab : Temp.label, s: string}

  local exception NotImplemented
  in
  
  (* nFormals is number of formal parameters *)
  (* calculate how the parameter will be seen inside function, frame or reg? *)
  (* what instructions must be producedto implement view shift? *)

  (* QUESTION: How to add data to string? *)
  fun str(lab, s) = DATA({lab=lab,s=s})

  (* sets up all params plus static link *)
(*  fun printOS(l::ls) = print(Int.toString(l)^"\n"); printOS(ls)
    | printOS(nil) = () *)

  fun newFrame(n) = 
    let
      fun createOffsets(0) = [Register.paramBaseOffset]
        | createOffsets(n) = Register.paramBaseOffset+ws*(n)::createOffsets(n-1)
      val oslst = rev (createOffsets(n))
    in
      ({formals=n+1, offlst= oslst, locals=ref 0, maxargs=ref 0}, oslst)
    end
  
  fun allocInFrame({formals, offlst, locals, maxargs}) = 
        Register.localsBaseOffset - ws*(!locals)

  fun incrLocal({formals, offlst, locals, maxargs}, newOffset) = 
    let val newOfflst = offlst @ [newOffset]
    in (locals := (!locals)+1; 
       {formals=formals, offlst=newOfflst, locals=locals, maxargs=maxargs})
    end

  fun increMax({formals, offlst, locals, maxargs}) = 
        (maxargs := (!maxargs)+1;
        {formals=formals, offlst=offlst, locals=locals, maxargs=maxargs})

  fun updateMaxargs({formals,offlst,locals,maxargs},newMax) =
        if newMax > !maxargs then maxargs := newMax
        else ()

  (* putting this function here to isolate system specific calls from Translate *)
  fun makeFrag({name, body, frame}) = PROC({name=name,body=body,frame=frame})

  fun externalCall(s, args) = Tree.CALL(Tree.NAME(Temp.namedlabel s), args)

  end

end (* structure Frame *)