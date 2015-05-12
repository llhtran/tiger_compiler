(* CPSC 421 Compilers and Interpreters Spring 2015*)
(* Student: Lien Tran BR 2016 *)
(* Assignment #5 due March 4th 2015 *)

signature ENV =
sig
  type access
  type level
  type label
  type ty

  datatype enventry 
    = VARentry of {access: access, ty: ty, iter: bool}
    | FUNentry of {level: level, label: label, formals: ty list, result: ty}

  type tenv = ty Symbol.table
  type env = enventry Symbol.table

  val base_tenv : tenv
  val base_env : env
end

structure Env : ENV =
struct

  structure S = Symbol
  structure T = Types

  type access = unit   (* not used for the time being *)
  type level = unit    (* not used for the time being *)
  type label = unit    (* not used for the time being *)
  type ty = T.ty

  datatype enventry 
    = VARentry of {access: access, ty: ty, iter: bool}
    | FUNentry of {level: level, label: label, formals: ty list, result: ty}

  type tenv = ty Symbol.table

  type env = enventry Symbol.table

(* TODO: Holy cow understand this function that I just wrote? *)
  fun fold f init nil = init
      | fold f init ((sym,ty)::ls) = f(fold f init ls, sym, ty)

  (* here you need to add all primtive types into the base_tenv *)
  val base_tenv = fold S.enter S.empty [
        (Symbol.symbol "int", T.INT),
        (Symbol.symbol "string", T.STRING),
        (Symbol.symbol "nil", T.NIL)]
 
  (* here you need to add all primitive library functions into the base_env *)
  val base_env = (List.foldl (fn ((s,ty),env) =>
                    S.enter(env, s, ty)) S.empty 
      [(S.symbol "print",FUNentry{level=(),label=(),formals=[T.STRING],result=T.UNIT}),
       (S.symbol "concat",FUNentry{level=(),label=(),formals=[T.STRING,T.STRING],result=T.STRING}),
       (S.symbol "not",FUNentry{level=(),label=(),formals=[T.INT],result=T.INT}),
       (S.symbol "flush",FUNentry{level=(),label=(),formals=nil,result=T.UNIT}),
       (S.symbol "chr",FUNentry{level=(),label=(),formals=[T.INT],result=T.STRING}),
       (S.symbol "getchar",FUNentry{level=(),label=(),formals=nil,result=T.STRING}),
       (S.symbol "substring",FUNentry{level=(),label=(),formals=[T.STRING,T.INT,T.INT],result=T.STRING}),
       (S.symbol "ord",FUNentry{level=(),label=(),formals=[T.STRING],result=T.INT}),
       (S.symbol "size",FUNentry{level=(),label=(),formals=[T.STRING],result=T.INT}),                                           
       (S.symbol "exit",FUNentry{level=(),label=(),formals=[T.INT],result=T.UNIT})])

end  (* structure Env *)
  
