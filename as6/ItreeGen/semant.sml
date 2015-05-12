(* semant.sml *)
(* CPSC 421 Compilers and Interpreters Spring 2015 *)
(* Student: Lien Tran BR 2016 *)

signature SEMANT =
sig
  type ir_code
  val transprog : Absyn.exp -> Frame.frag list
end

(* 

High level advice: 
What matters most is give Shao a working compiler. 
Need to work with two other groups.

On available documentation: esp for CodeGen group
Need to learn x86 ASSEMBLY
README has example of 2 files calling each other using .c and .s 
bar.s is x86 ASSEMBLY file
foo.c must call Tiger main rather than f()
runtime.c string in Tiger is different from C string, so have some functions
  to deal with these (string struct, compare string function, print string)
  consts - support conversion? Not sure what he said here exactly

Debugging tips:
Compile "Hello World" first!

gcc -s generate helloworld.c in ASSEMBLY? 

*)

(* parametrized module, called functor? *)
functor SemantGen (Register: REGISTER_STD) : SEMANT =

struct

  structure A = Absyn
  structure Translate = TranslateGen(Register)
  structure E = EnvGen(Translate)
  structure S = Symbol
  structure T = Types
  structure F = Format
  val error = ErrorMsg.error
  val breakCnt = ref [0]
  val HACKsym = S.symbol("")
  val HACK = ref ()   (* unique for keeping track of FOR loop counter types *)
  type ir_code = unit (* not used for the time being *)

  (********************************************
   *              Error Messages              *
   ********************************************)

  (* Expression errors *)
  val msgAppExp01  = "Type mismatch in parameter "
  val msgAppExp02  = "Function called with wrong number of parameters."
  val msgAppExp03  = "Identifier is not bound to a function.  Assuming function returns INT."
  val msgAppExp04  = "Undefined identifier in function call.  Assuming function returns INT."
  val msgArithExp01= "Left operand of arithmetic expression does not evaluate to an INT."
  val msgArithExp02= "Right operand of arithmetic expression does not evaluate to an INT."
  val msgArrayExp01= "Type of array size does not evaluate to an INT."
  val msgArrayExp02= "Type of array initializer does not agree with array declaration."
  val msgArrayExp03= "Attempt to assign a variable to a non-array type."
  val msgArrayExp04= "Attempt to declare an array of an unknown type."
  val msgAssignExp01= "Attempt to assign to a FOR loop induction variable."
  val msgAssignExp02= "Type of rvalue in assignment expression does not match lvalue."
  val msgBreakExp  = "BREAK expression found outside of WHILE or FOR loop."
  val msgCmpExp01  = "Illegal type for order comparison "
  val msgCmpExp02  = "Type mismatch in order comparison "
  val msgForExp01  = "LO condition in FOR statement does not evaluate to an INT."
  val msgForExp02  = "HI condition in FOR statement does not evaluate to an INT."
  val msgForExp03  = "Body expression in FOR statement does not evaluate to UNIT."
  val msgIfExp01   = "Test expression in IF statement does not evaluate to an INT."
  val msgIfExp02   = "THEN expression in IF..THEN statement does not evaluate to UNIT."
  val msgIfExp03   = "Type mismatch between THEN and ELSE clauses.  Assuming expression is an INT."
  val msgOpEqExp01 = "Cannot compare NIL with NIL (\"=\")."
  val msgOpEqExp02 = "Type mismatch in equality comparison \"=\"."
  val msgOpNeqExp01= "Cannot compare NIL with NIL (\"<>\")"
  val msgOpNeqExp02= "Type mismatch in inequality comparison \"<>\"."
  val msgRecordExp01= "Attempt to create an undefined record type."
  val msgRecordExp02= "Record type specifier is not defined as a record type."
  val msgRecordExp03= "Expected binding for field `%s', found binding for `%s'."
  val msgRecordExp04 = "Type of field `%s' initializer does not agree with record declaration."
  val msgRecordExpMissingField = "No binding found for field `%s'."
  val msgRecordExpExtraField = "Extraneous binding of field `%s'"

  val msgWhileExp01= "Test expression in WHILE statement does not evaluate to an INT."
  val msgWhileExp02= "Body expression in WHILE statement does not evaluate to UNIT."

  (* Var errors *)
  val msgH0x       = "Undefined type in variable expression."
  val msgH00       = "Identifier is not of valid lvalue type, it is a function name ("
  val msgH01       = "Variable ("
  val msgH01b      = ") is of an unknown type, or the variable has not been defined."
  val msgH02       = "Attempt to access an undefined record field ("
  val msgH03       = "Attempt to access a field of a non-record type."
  val msgH04       = "Expression for array index does not resolve to an INT."
  val msgH05       = "Attempt to index into a non-array variable."

  (* Utility function errors *)
  val breakFatalMsg= "FATAL ERROR. While/For scoping counter stack exhausted "
  val checkFuncs01 = "Formal parameter in function declaration is of an undeclared type."
  val checkFuncs02 = "Result type in function declaration is of an undeclared type."
  val checkFuncs03 = "Result type of function body does not match declared return type."
  val getFormalList01 = "Undeclared type in formal parameter list of function declaration ("
  val getFormalList02 = ")."
  val msgAddFunc01 = "Undeclared result type in function declaration.  Assuming function returns an INT."
  val msgAddFunc02 = "Function declaration makes use of an identifier already defined for this function list ("
  val msgAddType01 = "Type declaration makes use of an identifier already defined for this list of types ("
  val msgCheckTypes01="can't find type."
  val msgCheckTypes02="funky error."
  val msgCheckInt  = "Type mismatch, expected an INT. Replacing with INT."
  val msgCheckUnit = "Expected type of UNIT.  Replacing with UNIT."
  val msgNameFollow= "Primitive (non-array/record) cycle detected in recursive type definition; Type forced to INT. ("
  val msgtransty01 = "Attempt to declare an array of an unknown type."
  val msgtransty02 = "Attempt to declare a variable of an unknown type."
  val msgtransty03 = "Record field "
  val msgtransty04 = " is of an unknown type."
  val transdec01   = "Type mismatch between variable declaration and initialization expression."
  val transdec02   = "Type declaration of an unknown type."
  val transdec03   = "Illegal assignment of NIL to declaration of a variable of an unknown type."

  (*************************************************************************
   *                       UTILITY FUNCTIONS                               *
   *************************************************************************)

  fun checkInt ({exp=_, ty=T.INT},_,_) = ()
  | checkInt ({exp=_, ty=T.RECORD(s,u)},pos,"") = if u=HACK then () else error pos msgCheckInt
  | checkInt ({exp=_, ty=T.RECORD(s,u)},pos,errMsg) = if u=HACK then () else error pos errMsg
  | checkInt (_,pos,"") = error pos msgCheckInt
  | checkInt (_,pos,errMsg) = error pos errMsg

  fun checkUnit({exp=_, ty=T.UNIT},_,_) = ()
  | checkUnit(_,pos,"") = error pos (msgCheckUnit)
  | checkUnit(_,pos,errMsg) = error pos (errMsg)

  fun actual_ty_helper (T.NAME(s,ty),pos) = 
  (case !ty of 
      SOME typ => actual_ty_helper(typ,pos)
    | NONE => (error pos ("invalid type for identifier" ^ S.name s);  
              T.UNIT))                 
 | actual_ty_helper (ty,pos)  = ty

  fun actual_ty (SOME(ty:T.ty),pos) = actual_ty_helper (ty,pos)
   |  actual_ty (NONE,pos) = (error pos ("undeclared identifier"); T.UNIT)

(**
  * The memory cell breakCnt keeps a list of loop depth counters.
  * Each time we enter a loop, the head counter is incremented, and
  * decremented when we leave.  If a break expression is encountered
  * and the counter is zero, then we have an illegally placed break.
  * Whenever we enter a function declaration, we have the start the
  * count over again, so a new counter is added to the list and initialized
  * to zero.  It is removed after the function declaration.
  *)
  fun   breakCntHelper(x::xs,d) = ((x+d)::xs)
  |   breakCntHelper(nil,_) = raise Fail(breakFatalMsg^" (inc/dec).\n")
  fun   popBreakCntHelper(x::xs) = breakCnt:=xs
  |   popBreakCntHelper(nil) = raise Fail(breakFatalMsg^" (pop).\n")
  fun pushBreakCntHelper(xs) = breakCnt:=(0::xs)
  fun incBreakCnt() = (breakCnt:=breakCntHelper(!breakCnt,1); ())
  fun decBreakCnt() = (breakCnt:=breakCntHelper(!breakCnt,~1); ())
  fun popBreakCnt() = popBreakCntHelper(!breakCnt)
  fun pushBreakCnt() = pushBreakCntHelper(!breakCnt)

(**
  * isField(st,id) checks to see if id matches one of the fields in
  * st (symbol*ty list).  Returns NONE if there is no match
  *)
  fun isField((id,t)::remainder,matchId) =
    (
      if S.name(id) = S.name(matchId) then
        SOME(t)
      else
        isField(remainder,matchId)
    )
  |   isField(nil,_) = NONE

(**
  * Search a string list for an instance of name
  *)
  fun nameExists(name,x::xs) =
    (
      if (x=S.name(name)) then
        true
      else
        nameExists(name,xs)
    )
  |   nameExists(_,nil) = false

(**
  * Given a T.ty, if it is a name, follow it until you find out what
  * the actual type cound to it is.  If not a name, or if the chain of
  * names has been exhausted, just return the original type.  This
  * function also keeps track of identifiers it has seen in a chain,
  * so it can detect cycles if they exist.
  *)
  fun nameFollowHelper(tenv,ty,addedList,pos) =
    (
      case ty of
        T.NAME(s,to1) =>
          (
            let
              val workingList =
              (
                if nameExists(s,addedList) then
                  (error pos (msgNameFollow^S.name(s)^").");to1:=SOME(T.INT); addedList)
                else (S.name(s)::addedList)
              )
            in
              case (!to1) of
                SOME(T.NAME(n,u)) =>
                (
                  case !u of
                    SOME(tY) => nameFollowHelper(tenv,tY,workingList,pos)
                  | NONE => SOME(ty)
                )
              | SOME(otherType) => SOME(otherType)
              | NONE => SOME(ty)
            end
          )
      | t => SOME(t)
    )

(**
  * Wrapper for above function.
  *)
  fun nameFollow(tenv,ty,pos) = nameFollowHelper(tenv,ty,nil,pos)

(**
  * Compare two types for type equality, and return the type (as an option).
  * Return NONE if types are not equal.  Name types are first handled by
  * resolving to whatever type it is that they are supposed to resolve to.
  *)
  fun tyCmp(tenv,ty1,ty2,pos) =
    (
      case (nameFollow(tenv,ty1,pos),nameFollow(tenv,ty2,pos)) of
        (NONE,_) => NONE
      | (_,NONE) => NONE
      | (SOME(T.INT),SOME(T.RECORD(s,u))) => if u=HACK then SOME(T.INT) else NONE
      | (SOME(T.RECORD(s,u)),SOME(T.INT)) => if u=HACK then SOME(T.INT) else NONE
      | (SOME(T.INT),SOME(T.INT)) => SOME(T.INT)
      | (SOME(T.NIL),SOME(T.NIL)) => SOME(T.NIL)
      | (SOME(T.STRING),SOME(T.STRING)) => SOME(T.STRING)
      | (SOME(T.UNIT),SOME(T.UNIT)) => SOME(T.UNIT)
              (* arrays can only be compared if same declaration *)
      | (SOME(T.ARRAY(t1,u1)),SOME(T.ARRAY(t2,u2))) => if u1 <> u2 then NONE else SOME(T.ARRAY(t1,u1))
              (* records can only be compared if same declaration *)
      | (SOME(T.RECORD(s1,u1)),SOME(T.RECORD(s2,u2))) => if(u1<> u2) then NONE else SOME(T.RECORD(s1,u1))
      | (SOME(T.RECORD(s,u)),SOME(T.NIL)) => SOME(T.RECORD(s,u))
      | (SOME(T.NIL),SOME(T.RECORD(s,u))) => SOME(T.RECORD(s,u))
      | (SOME(_),SOME(_)) => NONE
    )

(**
  *  Create a (T.ty list) from a (A.formals list)
  *)
  fun getFormalList(tenv,{var,typ,pos}::params) =
    (
      case S.look(tenv,typ) of
        SOME(t) => t::getFormalList(tenv,params)
      | NONE => ((error pos (getFormalList01^S.name(typ)^getFormalList02); T.INT::getFormalList(tenv,params)))
    )
  |   getFormalList(_,nil) = nil

(**
  * Go through a list of possibly mutually recursive function declarations
  * and add E.FUNenrty's for each one.  The list addedList keeps a list of a
  * identifiers that have been used in this function declaration list, and
  * checks for repeated usage.
  *)
  fun addFuncNames(env,tenv,{name,params,result,body,pos}::decs,addedList,level,nameLst) =
    (
      let
        val resultTy =
        (
          case result of
            SOME(id,pos) =>
            (
              case S.look(tenv,id) of
                SOME(ty) => ty
              | NONE => (error pos msgAddFunc01; T.INT)
            )
          | NONE => T.UNIT
        )
        val newName = Temp.newlabel()
        val newLevel = Translate.newLevel({parent=level, formals=params})
        val fmlLst = getFormalList(tenv,params)     
        val env' = S.enter(env,name,E.FUNentry{level=newLevel,
                                               label=(newName),
                                               formals=fmlLst,
                                               result=resultTy})
       (* val env'' = foldl enterparam env' params'*)
        val workingList =
        (
          if nameExists(name,addedList) then
            (error pos (msgAddFunc02^S.name(name)^")"); addedList)
          else
            (S.name(name)::addedList)
        )
      in
        addFuncNames(env',tenv,decs,workingList,level,nameLst@[newName])
      end
    )
  |   addFuncNames(env,_,nil,_,_,nameLst) = (env, nameLst) (* TODO: change last to nil to be sure *)

(**
  * Go through a list of possibly mutually recursive type declarations
  * and add T.ty's for each one.  The list addedList keeps a list of a
  * identifiers that have been used in this type declaration list, and
  * checks for repeated usage.
  *)
  fun addTypeNames(env,tenv,{name,ty,pos}::decs,addedList) =
    (
      let
        val tenv' = S.enter(tenv,name,T.NAME(name,ref NONE))
        val workingList =
        (
          if nameExists(name,addedList) then
            (error pos (msgAddType01^S.name(name)^")"); addedList)
          else
            (S.name(name)::addedList)
        )
      in
        addTypeNames(env,tenv',decs,workingList)
      end
    )
  |   addTypeNames(_,tenv,nil,_) = tenv


 (*****************************************************************
  *                   TRANSLATING TYPE EXPRESSIONS                *
  *                                                               *
  *              transty : (E.tenv * A.ty) -> T.ty)               *
  *****************************************************************)
  fun transty (tenv, A.ArrayTy(id, pos)) =
    (
      case S.look(tenv,id) of
        SOME(t) => T.ARRAY(t,ref ())
      | NONE => (error pos msgtransty01; T.INT )
    )
  |   transty (tenv, A.NameTy(id,pos)) =
    (
      case S.look(tenv,id) of
        SOME(t) => t
      | NONE => (error pos msgtransty02; T.INT )
    )
  |   transty (tenv, A.RecordTy(tlist)) =
    (
      let
          fun recConv({name,typ,pos}::fields) =
          (case S.look(tenv,typ) of
            SOME(t) =>(name,t)::recConv(fields)
          | NONE => ((error pos (msgtransty03^S.name(name)^msgtransty04); (name,T.INT)::recConv(fields)))
          )
        | recConv(nil) = nil
      in
        T.RECORD(recConv(tlist), ref())
      end
    )

(**
  * The name checkTypes is just for symmetry with checkFuncs.  What it does is
  * to walk through the type definitions and bind the unique memory cell to
  * an actual type.  The actual type may end up being a T.NAME, but the rest
  * of the code can follow through the pointers after they are resolved.
  *)
  fun checkTypes(tenv,{name,ty,pos}::decs) =
      let
        val t = S.look(tenv,name)
      in
        case t of
          NONE => (error pos msgCheckTypes01)
        | SOME(tmp as T.NAME(s,u)) =>
            let
              val typ = transty(tenv,ty)
            in
              case typ of
                T.NAME(s0,u0) => u:=SOME(typ)
              | actType => u:=SOME(actType)
            end
        | _ => error pos msgCheckTypes02;
        checkTypes(tenv,decs)
      end
  |   checkTypes(tenv,nil) = tenv

(**
  * After all of the type definitions are entered, we run through all
  * of the entries one more time to check for cycles (using the nameFollow
  * function).  If we ever encounter a cycle, we break it by changing the
  * type of the type declaration that closes the loop into a T.INT.  This
  * could cause other problems later on of course.
  *)
  fun checkCycles(tenv,{name,ty,pos}::decs) =
    (
      nameFollow(tenv,transty(tenv,ty),pos);
      checkCycles(tenv,decs)
    )
  | checkCycles(_,nil) = ()


 (**************************************************************************
  *                   TRANSLATING EXPRESSIONS                              *
  *                                                                        *
  *  transexp : (E.env * E.tenv) -> (A.exp -> {exp : ir_code, ty : T.ty})  *
  **************************************************************************)
  fun transexp (env:E.env, tenv:E.tenv, level:Translate.level, done:Temp.label) expr =
    let fun g (A.NilExp) = {exp=(Translate.nilExp()),ty=T.NIL}
    | g (A.IntExp e) = {exp=(Translate.intExp(e)),ty=T.INT}
    | g (A.StringExp (strg,_)) = {exp=(Translate.strExp(strg)),ty=T.STRING}
    | g (A.AppExp {func,args,pos}) = 
          let
            fun extract(nil,le,lt) = (le,lt)
              | extract(l::ls,le,lt) = 
                  let val a = g l
                  in extract(ls,le @ [#exp(a)],lt @ [#ty(a)])
                  end
            val (le,lt) = extract(args,nil,nil)
          in  
    {exp=
     (
        case S.look(env,func) of
          NONE => (error pos msgAppExp04; Translate.nilExp())
        | SOME(E.FUNentry{level=flevel,label=label,formals=formals,result=result}) => 
              (*Translate.appExp({parent=level, funLevel=flevel, fname=label, args=le})*)
              Translate.appExp(flevel, level, label, le)
        | SOME(_) => (error pos msgAppExp04; Translate.nilExp())
     ),
     ty=
      case S.look(env,func) of
          SOME(E.FUNentry{level,label,formals,result}) =>
          (
            let
              fun checkArgs(A::As,f::fs,n) =
              (
                case tyCmp(tenv,A,f,pos) of
                  NONE => (checkArgs(As,fs,n+1); error pos (msgAppExp01^Int.toString(n)^"."))
                | SOME(_) => checkArgs(As,fs,n+1)
              )
              | checkArgs(nil,nil,_) = ()
              | checkArgs(_,_,_) = error pos msgAppExp02
            in
              checkArgs(lt,formals,1);
              result
            end
          )
        | SOME(E.VARentry{access,ty}) => (error pos msgAppExp03; T.INT)
        | NONE => (error pos msgAppExp04; T.INT)
    } end

    | g (A.RecordExp{fields,typ,pos}) = let
          fun chkField (symbol,exp,pos) = (symbol, #ty(g exp))

          fun cmpField ([], []) = ()
            | cmpField ((fieldSym,_)::fs, []) =
                (error pos (F.format msgRecordExpMissingField
                                     [F.STR (S.name fieldSym)]);
                 cmpField (fs, []))
            | cmpField ([], (sym,exp,pos)::es) =
                (error pos (F.format msgRecordExpExtraField
                                     [F.STR (S.name sym)]);
                 cmpField ([], es))
            | cmpField ((fieldSym,fieldTy)::fs, (sym,exp,pos)::es) =
              let val {ty,...} = g exp
              in
                  if fieldSym <> sym
                  then error pos (F.format msgRecordExp03
                                           [F.STR (S.name fieldSym),
                                            F.STR (S.name sym)])
                  else if isSome(tyCmp(tenv,fieldTy,ty,pos))
                       then ()
                       else error pos (F.format msgRecordExp04
                                                [F.STR (S.name fieldSym)]);
                  cmpField (fs,es)
              end

          fun transList (nil) = nil
            | transList ((sym,exp,pos)::lst) = (#exp (g exp))::transList(lst)

          val ty =
              (case S.look (tenv, typ) of
                   SOME t => nameFollow(tenv, t, pos)
                 | NONE => NONE)
      in
          case ty of
            SOME(t as T.RECORD(symTyList,uniq)) =>
              (cmpField (symTyList, fields);
               {exp=(Translate.recExp(transList(fields))), ty=t})
          | x =>
              (error pos (if isSome x
                          then msgRecordExp02
                          else msgRecordExp01);
               {exp=(Translate.nilExp()), ty=T.RECORD(map chkField fields, ref())}
               )
        end
    | g (A.SeqExp exprs) = 
         let
          val lastTy = ref T.UNIT;
          fun checkSeq(nil) = !lastTy
            | checkSeq(t::ls) = (lastTy:=t;checkSeq(ls);!lastTy)
          fun extract(nil,le,lt) = (le,lt)
            | extract((l,p)::ls,le,lt) = 
                  let val a = g l
                  in extract(ls,le @ [#exp(a)],lt @ [#ty(a)])
                  end
          val (le,lt) = extract(exprs,nil,nil)
         in
          {exp=(Translate.seqExp(le)), ty=checkSeq(lt)}
         end
    | g (A.IfExp {test,then',else',pos}) =     (* IF..THEN *)
      let 
          val {exp=then_Ex,ty=thenty}=g then'
          val {exp=else_Ex,ty=elsety}= (case else' of
                           SOME(exp) => g exp
                        |  NONE => 
                          (let
                            val ret = Translate.nilExp()
                          in
                            {exp=ret,ty=T.UNIT}
                          end))
          val testrec as {exp=test_Cx,ty=testty} = g test              
       in 
         (checkInt(testrec,pos,msgIfExp01));
         {exp=Translate.ifExp({test=test_Cx,then'=then_Ex,else'=else_Ex}),ty=thenty}
       end
(*)    (
      let 
        val newe1 = g e1
        val newe2 = g e2 
      in
        checkInt(newe1,pos,msgIfExp01);
        checkUnit(newe2,pos,msgIfExp02);
        {exp=(Translate.ifExp({test=(#exp(newe1)),then'=(#exp(newe2)),else'=NONE})),ty=T.UNIT}
      end
      (exp=(Translate.ifExp({test,else',NONE})),ty=T.UNIT}
    )
    | g (A.IfExp {test=e1,then'=e2,else'=SOME(elseexp),pos}) =  
    (
      checkInt(g e1,pos,msgIfExp01);
      case expCmp(e2,elseexp,pos) of
        SOME(t) => {exp=(Translate.ifExp({test=(#exp(g e1)),then'=(#exp(g e2)),else'=(SOME(#exp(g elseexp)))})),ty=t}
      | NONE => (error pos msgIfExp03; {exp=(Translate.nilExp()),ty=T.INT})
    )*)

    | g (A.WhileExp {test,body,pos}) =
    ( 
      incBreakCnt();
      (let
        val break = Temp.newlabel()
        val newtest = g test
        val newbody = transexp(env,tenv,level,break) body 
      in
        checkInt(newtest,pos,msgWhileExp01);
        checkUnit(newbody,pos,msgWhileExp02);
        decBreakCnt();
        {exp=(Translate.whileExp({test=(#exp(newtest)),body=(#exp(newbody)),done=break})),ty=T.UNIT}
      end)
    )

(* MEGA HACK *)

    | g (A.ForExp {var,lo,hi,body,pos}) =
    (
     let
        val i = A.SimpleVar(#name var,pos)
        val limit = A.SimpleVar(S.symbol "limit",pos)
        val decs= [A.VarDec{var=var,typ=NONE,init=lo,pos=pos},
        A.VarDec{var={name=S.symbol "limit",escape= ref true},typ=NONE,init=hi,pos=pos}]
        val increment = A.OpExp{left=A.VarExp i,oper=A.PlusOp,right=A.IntExp 1,pos=pos}
        val whilebody = A.SeqExp[(body,pos),
                        (A.AssignExp{var=i,exp=increment,pos=pos},pos)]
        val whileExp = A.WhileExp{test=A.OpExp{left=A.VarExp i,oper=A.LeOp,right=A.VarExp limit,pos=pos},
                                body=whilebody,pos=pos}
     in
         (g (A.LetExp{decs=decs,body=whileExp,pos=pos}))
     end   

    )
    | g (A.BreakExp(pos)) =
      if (hd(!breakCnt)=0)
      then (error pos msgBreakExp; {exp=(Translate.nilExp()),ty=T.UNIT})
      else {exp=(Translate.breakExp(done)),ty=T.UNIT}

    | g (A.LetExp{decs,body,pos}) =
      let
        val (env',tenv',level',explist) = transdecs(env,tenv,decs,level,nil,done)
        val result = transexp(env',tenv',level',done) body
      in
        case explist of 
          nil => result
        | _ => {exp=(Translate.letExp(explist, #exp(result))), ty= (#ty(result))}
      end

    | g (A.ArrayExp{typ,size,init,pos}) =   (* need to check size is int, and typeof(init) = typ *)
    (
      checkInt(g size,pos,msgArrayExp01);
      let
        val tenvEnt =
        (
          case S.look(tenv,typ) of
            SOME(t) => nameFollow(tenv,t,pos)
          | NONE => NONE
        )
      in
        case tenvEnt of
          SOME(T.ARRAY(ty,u)) =>
            let
              val initTy = (g init)
            in
              case tyCmp(tenv,ty,#ty(initTy),pos) of
                SOME(_) => {exp=(Translate.arrayExp(#exp(g size), #exp(g init))),
                            ty=T.ARRAY(#ty(initTy),u)}
              | NONE => (error pos msgArrayExp02; {exp=(Translate.nilExp()),ty=T.ARRAY(T.INT,ref ())})
            end
        | SOME(t) => (error pos msgArrayExp03; {exp=(Translate.nilExp()),ty=T.ARRAY(T.INT,ref())})
        | NONE => (error pos msgArrayExp04; {exp=(Translate.nilExp()),ty=T.ARRAY(T.INT,ref())})
      end
    )

(* [In]equality comparison operators apply to all types *)
    | g (A.OpExp {left,oper=A.NeqOp,right,pos}) =
    (
      let
        val t1 = #ty(g left)
        val t2 = #ty(g right)
      in
        case (t1,t2) of
          (T.NIL,T.NIL) => error pos msgOpNeqExp01
        | (_,_) => ();
        case expCmp(left,right,pos) of
          SOME(t) => 
          (
            case t of
              T.INT => {exp=(Translate.cmpExp(#exp(g left), A.NeqOp, #exp(g right))),ty=T.INT}
            | T.STRING => {exp=(Translate.strCmp(#exp(g left), A.NeqOp, #exp(g right))),ty=T.INT}
            | _ => {exp=(Translate.nilExp()),ty=T.INT} (* equality of other types, yo *)
          )
        | NONE => (error pos msgOpNeqExp02; {exp=(Translate.nilExp()),ty=T.INT})
      end
    )
      | g (A.OpExp {left,oper=A.EqOp,right,pos}) =
    (
      let
        val t1 = #ty(g left)
        val t2 = #ty(g right)
      in
        case (t1,t2) of
          (T.NIL,T.NIL) => error pos msgOpEqExp01
        | (_,_) => ();
        case expCmp(left,right,pos) of
          SOME(t) => 
          (
            case t of
              T.INT => {exp=(Translate.cmpExp(#exp(g left), A.EqOp, #exp(g right))),ty=T.INT}
            | T.STRING => {exp=(Translate.strCmp(#exp(g left), A.EqOp, #exp(g right))),ty=T.INT}
            | _ => {exp=(Translate.nilExp()),ty=T.INT} (* equality of other types, yo *)
          )
        | NONE => (error pos msgOpEqExp02; {exp=(Translate.nilExp()),ty=T.INT})
      end
    )
(* Order comparison operators <, >, <=, <= apply to INTs and STRINGs only *)
    | g (A.OpExp {left,oper=A.GeOp,right,pos}) =
    (
      case expCmp(left,right,pos) of
        SOME(t) =>
        (
          case t of
            T.INT => {exp=(Translate.cmpExp(#exp(g left), A.GeOp, #exp(g right))),ty=T.INT}
          | T.STRING => {exp=(Translate.strCmp(#exp (g left), A.GeOp, #exp (g right))),ty=T.INT} (* TODO *)
          | _ => (error pos (msgCmpExp01^"\">=\"."); {exp=(Translate.nilExp()),ty=T.INT})
        )
      | NONE => (error pos (msgCmpExp02^"\">=\"."); {exp=(Translate.nilExp()),ty=T.INT})
    )
    | g (A.OpExp {left,oper=A.GtOp,right,pos}) =
    (
      case expCmp(left,right,pos) of
        SOME(t) =>
        (
          case t of
            T.INT => {exp=(Translate.cmpExp(#exp(g left), A.GtOp, #exp(g right))),ty=T.INT}
          | T.STRING => {exp=(Translate.strCmp(#exp (g left), A.GtOp, #exp(g right))),ty=T.INT} (* TODO *)
          | _ => (error pos (msgCmpExp01^"\">\"."); {exp=(Translate.nilExp()),ty=T.INT})
        )
      | NONE => (error pos (msgCmpExp02^"\">\"."); {exp=(Translate.nilExp()),ty=T.INT})
    )
    | g (A.OpExp {left,oper=A.LeOp,right,pos}) =
    (
      case expCmp(left,right,pos) of
        SOME(t) =>
        (
          case t of
            T.INT => {exp=(Translate.cmpExp(#exp(g left), A.LeOp, #exp(g right))),ty=T.INT}
          | T.STRING => {exp=(Translate.strCmp(#exp (g left), A.LeOp, #exp (g right))),ty=T.INT}
          | _ => (error pos (msgCmpExp01^"\"<=\"."); {exp=(Translate.nilExp()),ty=T.INT})
        )
      | NONE => (error pos (msgCmpExp02^"\"<=\"."); {exp=(Translate.nilExp()),ty=T.INT})
    )
    | g (A.OpExp {left,oper=A.LtOp,right,pos}) =
    (
      case expCmp(left,right,pos) of
        SOME(t) =>
        (
          case t of
            T.INT => {exp=(Translate.cmpExp(#exp(g left), A.LtOp, #exp(g right))),ty=T.INT}
          | T.STRING => {exp=(Translate.strCmp(#exp (g left), A.LtOp, #exp (g right))),ty=T.INT}
          | _ => (error pos (msgCmpExp01^"\"<\"."); {exp=(Translate.nilExp()),ty=T.INT})
        )
      | NONE => (error pos (msgCmpExp02^"\"<\"."); {exp=(Translate.nilExp()),ty=T.INT})
    )
(* ADD,SUB,TIMES,DIVIDE apply only to INTs *)
    | g (A.OpExp {left,oper,right,pos}) =
      (
        checkInt (g left, pos, msgArithExp01);
        checkInt (g right, pos, msgArithExp02);
        {exp=(Translate.arthExp(#exp(g left), oper, #exp(g right))), ty=T.INT}
    )
    | g (A.VarExp var) = h(var)
    | g (A.AssignExp {var,exp,pos}) =
    (
      let
        val t1 = #ty(h var)
        val t2 = #ty(g exp)
      in
        case t1 of
          T.RECORD(s,u) => if u=HACK then (error pos msgAssignExp01) else ()
        | _ => ();
        case tyCmp(tenv,t1,t2,pos) of
          SOME(_) => {exp=(Translate.assignExp(#exp(h var), #exp(g exp))),ty=T.UNIT}
        | NONE => (error pos msgAssignExp02; {exp=(Translate.nilExp()),ty=T.UNIT})
      end
    )

(**
  * Function dealing with "var", mutually recursive
  *)
    and h (A.SimpleVar (id,pos)) =
    (
      let
        val envEnt = S.look(env,id)
      in
        case envEnt of
          SOME(E.VARentry{access,ty}) =>
          (
            case nameFollow(tenv,ty,pos) of
              SOME(t) => {exp=(Translate.simpleVar(access,level)),ty=t}
            | NONE => (error pos msgH0x; {exp=(Translate.nilExp()),ty=T.INT})
          )
        | SOME(E.FUNentry{level,label,formals,result}) => (error pos (msgH00^S.name(id)^")."); {exp=(Translate.nilExp()),ty=T.INT})
        | NONE => (error pos (msgH01^S.name(id)^msgH01b); {exp=(Translate.nilExp()),ty=T.INT})
      end
    )
    | h (A.FieldVar (v,id,pos)) =
    (
      let
          fun lookupFieldTyp (id,nil,pos,num)= 
              (error pos ("identifier " ^ S.name id ^ "not in record");(~1,T.UNIT))
           |  lookupFieldTyp (id,(x,y)::xs,pos,num)=
                if id=x 
                then (num,actual_ty_helper (y,pos))
                else lookupFieldTyp(id,xs,pos,num+1)
          val {exp=e,ty=typ}= h v
          val (num,fieldty)= 
            case typ of
              T.RECORD(tylist,u) => lookupFieldTyp(id,tylist,pos,0)
            | _ => (error pos "variable not a record"; (~1,T.UNIT))  
      
      in 
        if num = ~1 then let val ret = Translate.nilExp() in {exp=ret, ty=T.UNIT} end
        else {exp=Translate.fieldVar(e,Translate.intExp num), ty=fieldty}
      end
    )
    | h (A.SubscriptVar (v,exp,pos)) =
    (
      let
        val leftPiece = nameFollow(tenv,#ty(h v),pos)
      in
      (
          checkInt (g exp, pos, msgH04);
        case leftPiece of
          SOME(T.ARRAY(t,_)) => {exp=(Translate.subsVar(#exp(h v), #exp(g exp))),ty=t}
        | _ => (error pos msgH05; {exp=(Translate.nilExp()),ty=T.INT})
      )
      end
    )

(**
  * Compare two expressions for type equality, and return the type (as an option).
  * Return NONE if types are not equal.  This is mutually recursive so I don't have
  * to bother with passing env and tenv back and forth.
  *)
  and expCmp(exp1,exp2,pos) =
    let
      val t1 = g exp1
      val t2 = g exp2
    in
      tyCmp(tenv,#ty(t1),#ty(t2),pos)
    end
  in g expr


  end
 (** END of transexp **)

(**
  * Given a list of function declarations and environments that have been
  * appropriately `prepped' (see addFuncNames), adds the params to the
  * environment and type checks the function bodies.
  *)
  and checkFuncs(env,tenv,({name,params,result,body,pos}:A.fundec)::decs, level, nameLst,done) =
    let
      val newLvl = 
      (
        case S.look(env,name) of
          SOME(E.FUNentry{level=flevel,label=label,formals=formals,result=result}) => flevel
        | _ => (error pos "Cannot find function\n"; Translate.outermost)
      )
      val env' =
      (
        let
          val fmlOffset = rev (Translate.formals (length(params)))
          fun addParams(penv,{var,typ,pos}::params,offset::oslst) =
            let
              val t =
              (
                case S.look(tenv,typ) of
                  NONE => (error pos checkFuncs01; T.UNIT)
                | SOME(ty) => ty
              )
              val penv' = S.enter(penv,#name(var:A.vardec),E.VARentry{access=(newLvl,offset),ty=t})
            in
              addParams(penv',params,oslst)
            end
          | addParams(penv,nil,_) = penv
          | addParams(penv,_,nil) = penv
        in
          addParams(env,params,fmlOffset)
        end
      )
      val resultTy =
      (
        case result of
          NONE => T.UNIT
        | SOME(s,pos) =>
          (
            case S.look(tenv,s) of
              NONE => (error pos checkFuncs02; T.INT)
            | SOME(t) => t
          )
      )
    in
      let
        val {exp,ty} = transexp(env',tenv, newLvl,done) body
      in
        case tyCmp(tenv,ty,resultTy,pos) of
          NONE => error pos checkFuncs03
        | SOME(t) => ();
         Translate.exitFunc({tree=exp, flabel=hd(nameLst), level=newLvl});
         checkFuncs(env,tenv,decs,level, tl(nameLst),done)
      end
    end
  |   checkFuncs(_,_,nil,_,_,_) = () (* TODO: change last arg to nil to be sure *)


 (**************************************************************************
  *                   TRANSLATING DECLARATIONS                             *
  *                                                                        *
  *  transdec : (E.env * E.tenv * A.dec) -> (E.env * E.tenv)               *
  **************************************************************************)
  and transdec (env, tenv, A.VarDec{var,typ,init,pos}, explist, level,done) =
    let
      val initTy = transexp(env,tenv, level, done) init
      val newAccess = Translate.allocInFrame(level)
      val initTy' =
      (
        case typ of
          SOME(ty,pos) =>
          (
            case S.look(tenv,ty) of
              SOME(t) => t
            | NONE => #ty(initTy)
          )
        | NONE => #ty(initTy)
      )
    in
    (
      case typ of
        SOME(ty,pos) =>
        (
          case S.look(tenv,ty) of
            SOME(t) =>
            (
              case tyCmp(tenv,#ty(initTy),t,pos) of
                SOME(t') => ()
              | NONE => error pos transdec01
            )
          | NONE => error pos transdec02
        )
      | NONE => case #ty(initTy) of T.NIL => (error pos transdec03) | _ => ();
      (S.enter(env,#name(var),E.VARentry{access=(newAccess),ty=initTy'}), tenv, 
        explist @ [Translate.assignExp(Translate.simpleVar(newAccess,level), #exp(initTy))], level)
    )
    end
    | transdec (env, tenv, A.FunctionDec(declist), explist, level,done) =
    let
                (* gotta re-init loop depth counter *)
      val (env', fnameLst) = (pushBreakCnt(); addFuncNames(env,tenv,declist,nil,level,nil))   
    in
      checkFuncs(env',tenv,declist, level, fnameLst,done);
      popBreakCnt();    (* restore the loop depth counter *)
      (env', tenv, explist, level)
    end

    | transdec (env, tenv, A.TypeDec(declist),explist,level,done) =
    let
      val tenv' = addTypeNames(env,tenv,declist,nil)
      val tenv'' = checkTypes(tenv',declist)
    in
      checkCycles(tenv'',declist);
      (env, tenv'', explist, level)
    end


  (*** transdecs : (E.env * E.tenv * A.dec list * Translate.level) -> (E.env * E.tenv * Translate.level) ***)
  and transdecs (env,tenv,nil,level,explist,done) = (env,tenv,level,explist)
    | transdecs (env,tenv,dec::decs,level,explist,done) =
      let val (env',tenv',explist',level') = transdec (env,tenv,dec,explist,level,done)
      in transdecs (env',tenv',decs,level',explist',done)
      end

  (*** transprog : A.exp -> Frame.frag list ***)
  fun transprog prog =
      let val mainLevel = Translate.newLevel({parent=Translate.outermost, formals=[]})
          val name = Temp.namedlabel("tigermain")
          val {exp, ty} = transexp (E.base_env, E.base_tenv, mainLevel, name) prog
      in (Translate.exitFunc({tree=exp, flabel=name, level=mainLevel});
         Translate.getResult())
      end

end  (* structure Semant *)

