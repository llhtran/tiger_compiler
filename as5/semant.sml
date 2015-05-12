(* CPSC 421 Compilers and Interpreters Spring 2015*)
(* Student: Lien Tran BR 2016 *)
(* Assignment #5 due March 4th 2015 *)

signature SEMANT =
sig
  type ir_code
  val transprog : Absyn.exp -> {exp: ir_code, ty: Types.ty}
end

structure Semant : SEMANT = 
struct

  structure A = Absyn
  structure E = Env
  structure S = Symbol
  structure T = Types
  val error = ErrorMsg.error
  type ir_code = unit (* not used for the time being *)

  
  (*** FILL IN DETAILS OF YOUR TYPE CHECKER PLEASE !!! ***)

  val breakLvl = ref [ref 0] 

  (*************************************************************************
   *                       UTILITY FUNCTIONS                               *
   *************************************************************************)

  fun incrBrLvl() = (hd(!breakLvl) := !(hd(!breakLvl)) + 1) (* add to counter *)
  fun decrBrLvl() = (hd(!breakLvl) := !(hd(!breakLvl)) - 1) (* subtract from counter *)
  fun newBr() = (breakLvl := [ref 0] @ !breakLvl) (* add new counter for new scope *)
  fun delBr() = (breakLvl := tl(!breakLvl)) (* add new counter for new scope *)

  fun checkInt ({exp, ty}, pos) = 
    case ty of 
      T.INT => true
    | _ => false

  (* sameType takes two types can compares if they're the same *)
  fun sameType(ty1,ty2) = 
        (case (ty1,ty2) of 
          (T.NIL, T.NIL) => true
        | (T.INT, T.INT) => true
        | (T.STRING, T.STRING) => true
        | (T.ARRAY(ty1,key1), T.ARRAY(ty2,key2)) => 
            if key1 = key2 then true
            else false
        | (T.RECORD (lst1,u1), T.RECORD(lst2,u2)) =>
            if u1 = u2 then true
            else false 
        | (T.NIL, T.RECORD(lst,u)) => true
        | (T.RECORD(lst,u), T.NIL) => true
        | (T.NAME(id1, ref(SOME(t1))), T.NAME(id2, ref(SOME(t2)))) => sameType(t1,t2)
        | (T.NAME(id, ref(SOME(t))), typ2) => sameType(t, typ2)
        | (typ1, T.NAME(id, ref(SOME(t)))) => sameType(typ1, t) 
        | (T.UNIT, T.UNIT) => true
        | (_,_) => false)

  (* assuming that the lists have same length, checks if their
    corresponding elements are the same type *)
  fun sameList(ty1::ls1,ty2::ls2) = 
        if sameType(ty1,ty2) then sameList(ls1,ls2)
        else false
      | sameList(nil,nil) = true
      | sameList(_,_) = false

  (* this compares strings, not types *)
  fun existField(fname,(sym,ty)::ls) = 
        if S.name fname = S.name sym then true
        else existField(fname, ls)
      | existField(fname, nil) = false

  (* assume field already exists *)
  fun getField(fname,(sym,ty)::ls) = 
        if S.name fname = S.name sym then ty
        else getField(fname, ls)
      | getField(fname, nil) = T.UNIT (* should never get to this case *)  

  fun fold f init nil = init
      | fold f init ((sym,ty)::ls) = f(fold f init ls, sym, ty)

  (* for processing lst in records for type declarations *)
  fun getRecordTyLst(A.RecordTy(lst)) = lst
    | getRecordTyLst(_) = []

  fun check_ty (SOME(ty:T.ty),pos) = ty
    | check_ty (NONE,pos) = (ErrorMsg.error pos "Undeclared type."; T.UNIT)

  (* checks fr duplicates in a record lst *)
  fun dups nil = ()
    | dups (l::ls) = (map (fn y:A.tfield => 
        if #name(l) = #name(y) then (ErrorMsg.error (#pos(y)) ("Duplicate field: " ^ S.name (#name(y)))) 
        else ()) ls; dups ls)

  (* used to translate actual types for records *)
  fun transtyRec(tenv,nil) = nil
    | transtyRec(tenv,{name,typ,pos}::ls) = 
        [(name,check_ty(S.look(tenv,typ),pos))] @ transtyRec(tenv,ls) 

  (* this is the preprocessing function for type declarations *)
  fun preProcess(tenv, A.TypeDec({name,ty,pos}::ls)) = 
        let val tenv' = S.enter(tenv,name,T.NAME(name, ref NONE))
        in preProcess(tenv', A.TypeDec(ls))
        end
    | preProcess(tenv,_) = tenv

  (* checks for cycles in type declarations *)
  fun cycle (ty,lst) = 
        case ty of
          SOME(typ as T.NAME(name,t)) =>
            (case (List.find (fn x=>typ = x) lst) of
              SOME(x) => true
            | NONE => cycle(!t,lst@[typ]))
        | _ => false

  (* used for processing type declarations once everything has
    been preprocessed and all types exist in environment *)
  fun updateType(tenv,name,typ,pos) = 
      let val ty = check_ty(S.look(tenv,name),pos)
      in case ty of
          T.NAME(id,refty) => 
            (refty := SOME(typ);
            if cycle (SOME(ty),[]) then (ErrorMsg.error pos ("Cycle detected."); refty:=SOME(T.INT))
            else ())
        | _ => ()
      end

fun actual_ty_helper (T.NAME(s,ty),pos) = 
    (case !ty of 
        SOME typ => actual_ty_helper(typ,pos)
      | NONE => (error pos ("invalid type for identifier" ^ S.name s);  
                T.UNIT))                 
   | actual_ty_helper (ty,pos)  = ty

(* gets the actual type of a type *)
fun actual_ty (SOME(ty:T.ty),pos) = actual_ty_helper (ty,pos)
   |  actual_ty (NONE,pos) = (error pos ("Undeclared identifier."); T.UNIT)


 (**************************************************************************
  *                   TRANSLATING TYPE EXPRESSIONS                         *
  *                                                                        *
  *              transty : (E.tenv * A.ty) -> (T.ty * A.pos)               *
  *************************************************************************)

  fun transty (tenv, A.ArrayTy(id, pos)) = T.ARRAY(check_ty(S.look(tenv,id),pos), ref ())

    | transty (tenv, A.NameTy(id, pos)) = check_ty(S.look(tenv,id),pos)

    | transty (tenv, A.RecordTy(lst)) = (dups lst; T.RECORD(transtyRec(tenv,lst), ref ()))


 (**************************************************************************
  *                   TRANSLATING EXPRESSIONS                              *
  *                                                                        *
  *  transexp : (E.env * E.tenv) -> (A.exp -> {exp : ir_code, ty : T.ty})  *
  **************************************************************************)
  fun transexp (env, tenv) expr =
    let fun g (A.VarExp(e)) = h(e) 
          | g (A.NilExp) = {exp=(), ty=T.NIL}
          | g (A.IntExp(e)) = {exp=(), ty=T.INT}
          | g (A.StringExp(strg, pos)) = {exp=(), ty=T.STRING}

          | g (A.AppExp({func, args, pos})) = 
              let
                (* check if two formals lists have the same types *)
                fun sameListType(lst1,lst2) = 
                  if length(lst1) = length(lst2) then 
                    if sameList(lst1,lst2) then true
                    else false
                  else false
              in
                (* check if function is in env *)
                case S.look(env, func) of
                  NONE => (ErrorMsg.error pos ("Undefined function " ^ S.name func); {exp=(), ty=T.UNIT})
                | SOME(f) =>
                    case f of
                      E.FUNentry ({level=level,label=label,formals=fml,result=r}) => 
                        let 
                          fun getActTyLst (ty::ls) = [actual_ty_helper(ty,0)] @ getActTyLst(ls)
                            | getActTyLst (nil) = []
                          val templist = map #ty (map g args)
                          val arglist = getActTyLst(templist)
                        in
                          if sameListType(fml, arglist) then 
                              (*(if sameType(r,T.INT) then print("IsInt\n") 
                              else print ("not int\n");*){exp=(), ty=r}
                          else (ErrorMsg.error pos ("Unmatched arguments for " ^ S.name func); 
                                {exp=(), ty=r})
                        end
                    | _ => (ErrorMsg.error pos ("Unmatched arguments for " ^ S.name func); 
                            {exp=(), ty=T.UNIT})
              end

          | g (A.OpExp {left,oper=A.NeqOp,right,pos}) = 
              if testEquality((g left), (g right), pos) then {exp=(), ty=T.INT}
              else (ErrorMsg.error pos "Invalid inequality, unmatched types.";{exp=(), ty=T.UNIT})

          | g (A.OpExp {left,oper=A.EqOp,right,pos}) =
              if testEquality((g left), (g right), pos) then {exp=(), ty=T.INT}
              else (ErrorMsg.error pos "Invalid equality, unmatched types.";{exp=(), ty=T.UNIT})

          | g (A.OpExp {left,oper=A.PlusOp,right,pos}) = 
              mathOp ({left=left,oper=A.PlusOp,right=right,pos=pos})

          | g (A.OpExp {left,oper=A.MinusOp,right,pos}) = 
              mathOp ({left=left,oper=A.MinusOp,right=right,pos=pos})

          | g (A.OpExp {left,oper=A.TimesOp,right,pos}) = 
              mathOp ({left=left,oper=A.TimesOp,right=right,pos=pos})

          | g (A.OpExp {left,oper=A.DivideOp,right,pos}) = 
              mathOp ({left=left,oper=A.DivideOp,right=right,pos=pos})

          | g (A.OpExp {left,oper=A.LtOp,right,pos}) = 
              if testInequality (g left, g right, pos) then {exp=(), ty=T.INT}
              else (ErrorMsg.error pos "Invalid less then comparison, unmatched types.";
                    {exp=(), ty=T.UNIT})

          | g (A.OpExp {left,oper=A.GtOp,right,pos}) = 
              if testInequality (g left, g right, pos) then {exp=(), ty=T.INT}
              else (ErrorMsg.error pos "Invalid greater than comparison, unmatched types.";
                    {exp=(), ty=T.UNIT})

          | g (A.OpExp {left,oper=A.LeOp,right,pos}) = 
              if testInequality (g left, g right, pos) then {exp=(), ty=T.INT}
              else (ErrorMsg.error pos "Invalid less than or equal comparison, unmatched types.";
                    {exp=(), ty=T.UNIT})

          | g (A.OpExp {left,oper=A.GeOp,right,pos}) = 
              if testInequality (g left, g right, pos) then {exp=(), ty=T.INT}
              else (ErrorMsg.error pos "Invalid greater than or equal comparison, unmatched types.";
                    {exp=(), ty=T.UNIT})

          | g (A.RecordExp{fields,typ,pos}) = 
              let fun checkRecFields((sym,exp,pos)::fields, (rsym,ty)::ls) = (* returns a list *)
                        let val resolveExp = transexp (env,tenv) exp
                        in
                          if sameType(#ty(resolveExp),actual_ty_helper(ty,pos)) then 
                              (sym,actual_ty_helper(ty,pos))::checkRecFields(fields,ls)
                          else (ErrorMsg.error pos ("Record field type doesn't match with record type."); 
                                nil)
                        end
                  | checkRecFields(nil,nil) = nil
                  | checkRecFields(_,_) = nil (* should never get to this case *)
              in 
                case actual_ty(S.look(tenv, typ), pos) of 
                  T.RECORD(lst,u) => 
                    if length(fields) = length(lst) then 
                        {exp=(), ty=T.RECORD(checkRecFields(fields,lst), u)}
                    else (ErrorMsg.error pos "Incorrect record length."; {exp=(), ty=T.UNIT})
                | _ => (ErrorMsg.error pos "Not a record type."; {exp=(), ty=T.UNIT})
              end

          | g (A.SeqExp(lst)) = 
              let 
                fun seq ((exp,pos)::nil) = g exp
                | seq ((exp, pos)::ls) = (g exp; seq ls)
                | seq (nil) = {exp=(), ty=T.UNIT}
              in seq(lst)
              end

          | g (A.AssignExp {var,exp,pos}) = 
              let val loopAssign =               
                    case var of 
                      A.SimpleVar(id,pos) => 
                        (case S.look(env,id) of
                             SOME(E.VARentry{iter,...}) => iter
                          |  _ => true)    
                   |  _ => true
              in 
                if sameType(#ty(h var), #ty(g exp)) then 
                  if loopAssign then {exp=(), ty=T.UNIT}
                  else (ErrorMsg.error pos ("Illegal assignment to loop variable."); {exp=(), ty=T.UNIT})
                else (ErrorMsg.error pos ("Invalid assignment, unmatched type."); {exp=(), ty=T.UNIT})
              end

          | g (A.IfExp {test,then',else', pos}) = 
              let val run = g then'
              in if checkInt (g test, pos) then 
                    (case else' of
                      NONE => {exp=(),ty=T.UNIT}
                    | SOME(exp) => 
                        if sameType(#ty(run),#ty(g exp)) then {exp=(), ty=(#ty(run))}
                        else (ErrorMsg.error pos "If else unmatched type."; {exp=(), ty=T.UNIT}))
                  else (ErrorMsg.error pos "If condition test must be of type int."; {exp=(), ty=T.UNIT})
              end

          | g (A.WhileExp {test,body,pos}) = 
              (incrBrLvl();
               if checkInt (g test, pos) then 
                  (case #ty(g body) of 
                    T.UNIT => (decrBrLvl(); {exp=(),ty=T.UNIT})
                  | _ => (ErrorMsg.error pos "Body of while loop must produce no value."; 
                          decrBrLvl(); {exp=(), ty=T.UNIT}))
               else (ErrorMsg.error pos "While loop test must produce type int."; 
                          decrBrLvl(); {exp=(), ty=T.UNIT}))

          | g (A.ForExp {var,lo,hi,body,pos}) = 
              (incrBrLvl();
              let
                val (tempEnv, tempTenv) = (S.enter(env,(#name(var)), 
                  E.VARentry({access=(), ty=T.INT,iter=false})),tenv)
                val bodyResult = transexp (tempEnv, tempTenv) body
              in
                if checkInt(g lo, pos) andalso checkInt(g hi, pos) then
                  case #ty(bodyResult) of 
                    T.UNIT => (decrBrLvl(); {exp=(), ty=(#ty(bodyResult))})
                  | _ => (ErrorMsg.error pos "For loop cannot produce value."; 
                          decrBrLvl(); {exp=(), ty=T.UNIT}) 
                else (ErrorMsg.error pos "For loop conditions are not integers."; 
                      decrBrLvl(); {exp=(), ty=T.UNIT})
              end)


          | g (A.BreakExp(pos)) = 
              (if !(hd(!breakLvl)) = 0 then (ErrorMsg.error pos "Break not inside loop.") 
              else ();
              {exp=(), ty=T.UNIT})

          | g (A.LetExp {decs,body,pos}) = 
              let val (tempEnv, tempTenv) = transdecs (env, tenv, decs)
              in transexp (tempEnv, tempTenv) body
              end

          | g (A.ArrayExp {typ,size,init,pos}) = 
                (if checkInt(g size, pos) then  
                  (case actual_ty(S.look(tenv, typ),pos) of 
                    T.ARRAY(artyp,key) => 
                      if sameType(#ty(g init), artyp) then {exp=(),ty=T.ARRAY(artyp,key)}
                      else (ErrorMsg.error pos "Incorrect array initialization value."; {exp=(), ty=T.UNIT}) 
                  | _ => (ErrorMsg.error pos "Invalid array type"; {exp=(), ty=T.UNIT}))
                else (ErrorMsg.error pos "Array size is not an int."; {exp=(), ty=T.UNIT}))

        and h (A.SimpleVar (id,pos)) = 
              (case S.look(env, id) of 
                SOME(E.VARentry{access, ty,iter}) => {exp=(), ty=ty}
              | _ => (ErrorMsg.error pos ("Variable not found: " ^ S.name id); {exp=(), ty=T.UNIT}))

          | h (A.FieldVar (v,id,pos)) = 
              (case #ty(h v) of 
                T.RECORD(lst,key) => 
                  if existField(id,lst) then {exp=(), ty=getField(id,lst)}
                  else (ErrorMsg.error pos ("Record field not found: " ^ S.name id); {exp=(), ty=T.UNIT})
              | _ => (ErrorMsg.error pos ("Record not found for field: " ^ S.name id); {exp=(), ty=T.UNIT}))

          | h (A.SubscriptVar (v,exp,pos)) = 
            (if checkInt(g exp, pos) then 
              case #ty(h v) of
                T.ARRAY(typ,key) => {exp=(), ty=actual_ty_helper(typ,pos)}
              | _ => (ErrorMsg.error pos ("Not an array."); {exp=(), ty=T.UNIT})
            else (ErrorMsg.error pos ("Array subscript is not an int."); {exp=(), ty=T.UNIT}))

        and testInequality({exp=exp1, ty=ty1}, {exp=exp2, ty=ty2}, pos) = 
          (case (ty1, ty2) of
                (T.INT, T.INT) => true
              | (T.STRING, T.STRING) => true 
              | _ => false)
        
        and testEquality({exp=exp1, ty=ty1}, {exp=exp2, ty=ty2}, pos) = 
              (case (ty1, ty2) of
                (T.INT, T.INT) => true
              | (T.STRING, T.STRING) => true 
              | (T.ARRAY (typ1,u1), T.ARRAY (typ2,u2)) =>
                  if u1 = u2 then true
                  else false
              | (T.RECORD(lst1,u1), T.RECORD(lst2,u2)) =>
                  if u1 = u2 then true
                  else false 
              | (T.RECORD (lst1,u1), T.NIL) => true
              | (T.NIL, T.RECORD (lst1,key1)) => true
              | (T.NAME(id1, ref(SOME(t1))), T.NAME(id2, ref(SOME(t2)))) => 
                  testEquality({exp=exp1,ty=t1},{exp=exp2,ty=t2},pos)
              | (T.NAME(id, ref(SOME(t))), ty2) => 
                  testEquality({exp=exp1,ty=t},{exp=exp2,ty=ty2},pos)
              | (ty1, T.NAME(id, ref(SOME(t)))) => 
                  testEquality({exp=exp1,ty=ty1},{exp=exp2,ty=t},pos)
              | _ => false)

        and mathOp({left,oper,right,pos}) = 
              (checkInt (g left, pos);
              checkInt (g right, pos);
              {exp=(), ty=T.INT})

     in g expr
    end

 (**************************************************************************
  *                   TRANSLATING DECLARATIONS                             *
  *                                                                        *
  *  transdec : (E.env * E.tenv * A.dec) -> (E.env * E.tenv)               *
  **************************************************************************)
  and transdec (env, tenv, A.VarDec({var, typ=NONE, init, pos})) = 
        let val tyr = transexp (env, tenv) init
            val b = E.VARentry({access=(), ty=(#ty(tyr)),iter=true})
        in 
          case #ty(tyr) of 
            T.NIL => (ErrorMsg.error pos "Must specify type for nil variable."; (env, tenv))
          | T.UNIT => (ErrorMsg.error pos "Initializing exp is invalid."; (env, tenv))
          | _ => (S.enter(env, (#name(var)), b), tenv)
        end
      
      | transdec (env, tenv, A.VarDec({var,typ=SOME(sym,p), init, pos})) =
        let val inittyp = transexp (env, tenv) init
        in
            let val typx = actual_ty(S.look(tenv,sym),pos)
                val b = E.VARentry({access=(), ty=typx,iter=true})
            in 
              case #ty(inittyp) of 
                T.NIL => 
                  (case typx of 
                    T.RECORD(lst,u) => (S.enter(env, (#name(var)), b), tenv)
                  | _ => (ErrorMsg.error pos ("nil variables must be of type record."); (env,tenv)))
              | _ => 
                  if sameType(typx,#ty(inittyp)) then (S.enter(env, (#name(var)), b), tenv)
                  else (ErrorMsg.error pos ("Variable type and value do not match: " 
                        ^ S.name (#name(var))); (env, tenv))
            end
        end

      | transdec (env, tenv, A.FunctionDec(fdeclst)) = 
          (newBr();
          let 
            fun transparam ({var,typ,pos}:A.formals) = 
                ({name=(#name(var)), ty=actual_ty(S.look(tenv,typ),pos)})
            
            fun preProcFunc(env, A.FunctionDec({name,params,result,body,pos}::ls)) =
                  let val saidResult = 
                        case result of
                          SOME(s,p) => actual_ty(S.look(tenv,s),p)
                        | NONE => T.UNIT 
                      val params' = map transparam params
                      val fentry = E.FUNentry({formals=(map #ty params'),
                                    result=saidResult,level=(),label=()})
                      val env' = S.enter(env,name,fentry)
                  in preProcFunc(env',A.FunctionDec(ls))
                  end
              | preProcFunc(env,_) = env
            
            fun procFunc(env,tenv,{name,params,result,body,pos}::ls) = 
                  let 
                    val params' = map transparam params
                    fun enterparams({name,ty},venv) = 
                          S.enter(venv,name,E.VARentry{access=(),ty=ty,iter=true})
                    val venv' = foldl enterparams env params'
                    val bodyReturn = actual_ty_helper(#ty(transexp(venv',tenv) body), pos)
                    val resultReturn = 
                          case S.look(env,name) of 
                            SOME(E.FUNentry{result,...}) => result
                          | _ => T.UNIT
                  in 
                    (if sameType(bodyReturn,resultReturn) then ()
                    else ErrorMsg.error pos "Function return type does not match declaration.");
                    procFunc(env,tenv,ls)
                  end
                | procFunc(env,tenv,nil) = (env,tenv)

              val processed = procFunc(preProcFunc(env,A.FunctionDec(fdeclst)), tenv,fdeclst)
          in delBr(); processed
          end)

      | transdec (env, tenv, A.TypeDec(tydeclist)) = 
          let val tenv' = preProcess(tenv, A.TypeDec(tydeclist))
              fun getRealTypes (tenv, A.TypeDec({name,ty,pos}::ls)) =
                    let val typx = transty (tenv,ty)
                    in (updateType(tenv,name,typx,pos); getRealTypes(tenv,A.TypeDec(ls)))
                    end
                | getRealTypes (tenv,_) = tenv
          in (env, getRealTypes(tenv',A.TypeDec(tydeclist)))
          end

  (*** transdecs : (E.env * E.tenv * A.dec list) -> (E.env * E.tenv) ***)
  and transdecs (env,tenv,nil) = (env, tenv)
    | transdecs (env,tenv,dec::decs) =
        let val (env',tenv') = transdec (env,tenv,dec)
 	      in transdecs (env',tenv',decs)
        end

  (*** transprog : A.exp -> {exp : ir_code, ty : T.ty} ***)
  fun transprog prog = transexp (E.base_env, E.base_tenv) prog

end  (* structure Semant *)
  

