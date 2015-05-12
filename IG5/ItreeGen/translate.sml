(* translate.sml *)
(* CPSC 421 Compilers and Interpreters Spring 2015 *)
(* Student: Lien Tran BR 2016 *)

signature TRANSLATE = 
sig 
  type level
  type access
  type frag

  val outermost : level 
  val newLevel : {parent: level, formals: 'a list} -> level 
  val allocInFrame : level -> access

  val getResult : unit -> frag list

  type gexp
  
  val unEx : gexp -> Tree.exp
  val unNx : gexp -> Tree.stm
  val unCx : gexp -> (Temp.label * Temp.label -> Tree.stm)

  val exitFunc : {tree: gexp, flabel: Temp.label, level: level} -> unit

  val nilExp : unit -> gexp
  val intExp : int -> gexp
  val strExp : string -> gexp
  val appExp : level * level * Temp.label * gexp list -> gexp
  val recExp : gexp list -> gexp
  val seqExp : gexp list ->  gexp
  val assignExp : gexp * gexp -> gexp
  val ifExp : {test: gexp, then': gexp, else': gexp} -> gexp 
  val whileExp : {test: gexp, body: gexp, done: Temp.label} -> gexp
  val forExp : {acc: gexp, lo: gexp, hi: gexp, body: gexp} -> gexp
  val breakExp : Temp.label -> gexp
  val letExp : gexp list * gexp -> gexp
  val arrayExp : gexp * gexp -> gexp
  val cmpExp : gexp * Absyn.oper * gexp -> gexp
  val strCmp : gexp * Absyn.oper * gexp -> gexp
  val arthExp : gexp * Absyn.oper * gexp -> gexp
  val simpleVar : access * level -> gexp
  val fieldVar : gexp * gexp -> gexp
  val subsVar : gexp * gexp -> gexp 
  val formals : int -> int list

end (* signature TRANSLATE *)


functor TranslateGen(Register : REGISTER_STD) : TRANSLATE = 
struct

  structure F = Frame
  structure Tr = Tree
  structure T = Temp
  structure Er = ErrorMsg
  structure A = Absyn 

  datatype level = LEVEL of {frame : F.frame,              
                             sl_offset : int,
                             parent : level} * unit ref
                 | TOP 

  type access = level * F.offset 
  (* type access = level * F.access *)
  type frag = F.frag

  val fragmentlist = ref ([] : frag list)

  val outermost = TOP 

  fun newLevel ({parent=LEVEL({frame=pframe,sl_offset=psl_offset,parent=gparent}, u), formals=flst}) = 
    (*fun newLevel ({parent=parent, formals=flst}) =*)
        let val (newFr,lst) = F.newFrame(length(flst)) (* lst returned accounts for static link space *)
            val sl = Register.paramBaseOffset
        in F.updateMaxargs(pframe,length(flst));
           LEVEL({frame=newFr, 
                  sl_offset=sl, 
                  parent=LEVEL({frame=pframe,sl_offset=psl_offset,parent=gparent}, u)}, 
           ref ())
        end

    | newLevel ({parent=TOP, formals=flst}) = 
        let val (newFr,lst) = F.newFrame(length(flst)) 
        in LEVEL({frame=newFr, sl_offset=Register.paramBaseOffset, parent=TOP}, ref ())
        end

  (* varDec *)
  fun allocInFrame (LEVEL({frame, sl_offset, parent}, u)) = 
    let val newOffset = F.allocInFrame(frame)
        val newFrame = F.incrLocal(frame, newOffset)
    in (LEVEL({frame=newFrame, 
               sl_offset=sl_offset, 
               parent=parent}, u), 
        newOffset)
    end
    | allocInFrame (TOP) = (ErrorMsg.impossible "Allocating in nonexistent frame"; (TOP, 0))
  
  fun getResult () = 
    let
      val i = List.length(!fragmentlist)
      val temp = !fragmentlist
    in
      fragmentlist := []; temp
    end

  datatype gexp = Ex of Tr.exp
                | Nx of Tr.stm
                | Cx of T.label * T.label -> Tr.stm

  fun seq (a::nil) = a
    | seq (a::r) = Tr.SEQ(a, seq r)
    | seq [] = (print("Error: empty list fed to seq\n"); Tr.EXP(Tr.CONST 0))

  fun unEx(Ex e) = e (*let val l = Temp.newlabel() in Tr.ESEQ(Tr.LABEL l,e) end*)
    | unEx(Nx s) = Tr.ESEQ(s, Tr.CONST 0)
    | unEx(Cx genstm) = 
        let
          val r = T.newtemp()
          val t = T.newlabel() and f = T.newlabel()
        in Tr.ESEQ(seq[Tr.MOVE(Tr.TEMP r, Tr.CONST 1), 
                         genstm(t,f), 
                         Tr.LABEL f,
                         Tr.MOVE(Tr.TEMP r, Tr.CONST 0), 
                         Tr.LABEL t], 
                  Tr.TEMP r) 
        end

  and unNx(Ex e) = Tr.EXP(e)
    | unNx(Nx s) = s
    | unNx(Cx genstm) = (* test this, not sure how this works *)
        let val t = T.newlabel()
            val f = t 
        in Tr.SEQ(genstm(t,f), Tr.LABEL t)
        end
  
  and unCx(Ex (Tr.CONST(1))) = (fn (t,f) => Tr.JUMP(Tr.NAME t, [t])) (* QUESTION *)
    | unCx(Ex (Tr.CONST(0))) = (fn (t,f) => Tr.JUMP(Tr.NAME f, [f])) (* Why is this NAME? *)
    | unCx(Ex e) = 
        let val r = T.newtemp()
        in fn (t,f) => Tr.SEQ(Tr.MOVE(Tr.TEMP r, e), 
                       Tr.CJUMP(Tr.TEST(Tr.NE, Tr.CONST 0, Tr.TEMP r), t, f))
        end
    | unCx(Cx genstm) = genstm
    | unCx(Nx _) = (ErrorMsg.impossible "Cannot unCx an Nx"; fn (_) => Tr.EXP(Tr.CONST 0))

    
fun exitFunc({tree=tree, flabel:Temp.label, level=LEVEL({frame=frame, sl_offset=sl_offset, parent=parent}, u)}) =
        let
          val finalTree = Tr.MOVE(Tr.TEMP (Register.RV), unEx(tree))
        in
          (fragmentlist := !fragmentlist @ [F.makeFrag({name=flabel, body=finalTree, frame=frame})])
        end
    | exitFunc({tree, flabel, level=TOP}) = (ErrorMsg.impossible "ERROR: Cannot exit TOP level.\n"; ())


  fun nilExp () = Ex(Tr.CONST 0)
  fun intExp (e) = Ex(Tr.CONST e)
  fun strExp (s) = 
    let val lab = Temp.newlabel()
    in fragmentlist := !fragmentlist @ [F.str(lab, s)]; Ex(Tr.NAME lab)
    end

  fun leveldepth lvl =
    case lvl of TOP => 0 | LEVEL({parent,...},u) => 1+leveldepth(parent)

  fun addsl(depthdiff,definelvl as LEVEL({parent=p1,...},u1),uselvl as LEVEL({sl_offset,parent=p2,...},u2)) =
    if depthdiff =0  then 
      Tr.MEM
       (Tr.BINOP
         (Tr.PLUS,
         Tr.TEMP(Register.FP),
         Tr.CONST sl_offset),F.ws)
   else if depthdiff = 1 orelse depthdiff = ~1 then Tr.TEMP(Register.FP)  
   else Tr.TEMP(Register.FP)  
   | addsl(_,_,_) =Tr.TEMP(Register.FP)
  
  fun getSL(defineLvl as LEVEL({parent=p1 as LEVEL(_,t),sl_offset=s1,...}, target), 
            curLvl as LEVEL({parent=p2,sl_offset=s2,...}, u), 
            fp) =
        (if t=u then fp
        else getSL(defineLvl,p2, Tr.MEM(Tr.BINOP(Tr.PLUS, fp, Tr.CONST s2), F.ws)))
    | getSL(_,TOP,fp) = ErrorMsg.impossible "Die\n"
    | getSL(TOP,_,fp) = ErrorMsg.impossible "Die2\n"
    | getSL(defineLvl as LEVEL({parent=p1 as TOP,sl_offset=s1,...}, target), 
            curLvl as LEVEL({parent=p2,sl_offset=s2,...}, u), 
            fp) = (ErrorMsg.impossible "This is tigermain. How come there are declared functions here?\n"; fp)

  fun getSLlocals(defineLvl as LEVEL({parent=p1,sl_offset=s1,...}, target), 
            curLvl as LEVEL({parent=p2,sl_offset=s2,...}, u), 
            fp) =
        (if target=u then fp
        else getSLlocals(defineLvl,p2, Tr.MEM(Tr.BINOP(Tr.PLUS, fp, Tr.CONST s2), F.ws)))
    | getSLlocals(_,TOP,fp) = ErrorMsg.impossible "Die\n"
    | getSLlocals(TOP,_,fp) = ErrorMsg.impossible "Die2\n"

  fun appExp (defineLvl, useLvl, fname, args) =
        let val lst = map unEx args
        in
        case defineLvl of
        TOP => Ex(Tr.CALL(Tr.NAME(fname), lst)) 
      | LEVEL({frame,...},u) => (F.updateMaxargs(frame,length(args)+1); Ex(Tr.CALL(Tr.NAME(fname),
             [getSL(defineLvl, useLvl, Tr.TEMP (Register.FP))] @ lst))) end 
    
  fun seqExp(nil) = Ex(Tr.CONST 0)
    | seqExp([s]) = s
    | seqExp(s::lst) = Ex(Tr.ESEQ(unNx(s), unEx(seqExp(lst))))

  fun saverecordExp (nil,_,_) = []
    | saverecordExp (l::ls,offset, r) =
      [Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS,
                              Tr.TEMP r,
                              Tr.CONST (offset*F.ws)),
                      F.ws),
              unEx l)]
      @ saverecordExp (ls,offset+1,r)

  (* Allocate space for a new record *)
  fun recExp explist_Ex = 
    let 
      val r= T.newtemp()
    in
      Ex(Tr.ESEQ(
        seq([Tr.MOVE(Tr.TEMP r,F.externalCall("allocRecord",
                [Tr.CONST ((List.length explist_Ex) *(F.ws))]))] @
          saverecordExp (explist_Ex,0,r)),
        Tr.TEMP r))
    end

  fun ifExp({test,then',else'}) =
      let 
      val r = T.newtemp() 
      val t = T.newlabel() and f = T.newlabel()
      val join = T.newlabel()
    in    
      Ex(Tr.ESEQ(seq[unCx(test) (t,f),
                    Tr.LABEL t,
                    Tr.MOVE(Tr.TEMP r, unEx then'),
                    Tr.JUMP(Tr.NAME join,[join]),
                    Tr.LABEL f,
                    Tr.MOVE(Tr.TEMP r, unEx else'),
                    Tr.LABEL join],
                    Tr.TEMP r))
    end

  fun whileExp({test, body, done}) =
        let
          val t = T.newlabel()
          val r = T.newtemp()
        in 
           Nx(seq[unCx(test) (t,done),
                  Tr.LABEL t,
                  unNx(body), 
                  unCx(test) (t,done),
                  Tr.LABEL done])
        end

  fun forExp({acc,lo,hi,body=bodyExp}) = 
        let
          val done = T.newlabel()
          val var = unEx acc
          val init = unEx lo
          val limit = unEx hi
          val body = unNx bodyExp
          val bodyLabel = Temp.newlabel ()
          val incrLabel = Temp.newlabel ()
        in       Nx (seq [Tr.MOVE (var, init),
                 Tr.CJUMP (Tr.TEST(Tr.LE, var, limit), bodyLabel, done),
                 Tr.LABEL bodyLabel,
                 body,
                 Tr.CJUMP (Tr.TEST(Tr.LT, var, limit), incrLabel, done),
                 Tr.LABEL incrLabel,
                 Tr.MOVE (var, Tr.BINOP (Tr.PLUS, var, Tr.CONST 1)),
                 Tr.JUMP (Tr.NAME bodyLabel, [bodyLabel]),
                 Tr.LABEL done])
        end

  fun breakExp(done) = Nx(Tr.JUMP(Tr.NAME done, [done]))


  fun letExp (decs, body) = Ex(Tr.ESEQ(seq(map unNx decs),unEx body))
    
  (* creating an array: need to allocate space and put information in there,
     as well as put this number of things into frame as local variables *)
  fun arrayExp(size, init) = 
        Ex(F.externalCall("initArray", 
          [Tr.BINOP(Tr.PLUS,Tr.CONST 1,unEx(size)), unEx(init)]))
  (* runtime.c is changed to make the first element store the size of array *)

  fun cmpExp (left, A.EqOp, right) = 
        Cx(fn (t, f) => Tr.CJUMP(Tr.TEST(Tr.EQ, unEx(left), unEx(right)), t, f))
    | cmpExp (left, A.NeqOp, right) = 
        Cx(fn (t, f) => Tr.CJUMP(Tr.TEST(Tr.NE, unEx(left), unEx(right)), t, f))
    | cmpExp (left, A.LeOp, right) = 
        Cx(fn (t, f) => Tr.CJUMP(Tr.TEST(Tr.LE, unEx(left), unEx(right)), t, f))
    | cmpExp (left, A.GeOp, right) = 
        Cx(fn (t, f) => Tr.CJUMP(Tr.TEST(Tr.GE, unEx(left), unEx(right)), t, f))
    | cmpExp (left, A.LtOp, right) = 
        Cx(fn (t, f) => Tr.CJUMP(Tr.TEST(Tr.LT, unEx(left), unEx(right)), t, f))
    | cmpExp (left, A.GtOp, right) = 
        Cx(fn (t, f) => Tr.CJUMP(Tr.TEST(Tr.GT, unEx(left), unEx(right)), t, f))
    | cmpExp (left, _, right) = 
        ((ErrorMsg.impossible "Unsupported comparison operator\n");
        Cx(fn (t, f) => Tr.EXP(Tr.CONST 0))) 

  fun strCmp (left, A.GeOp, right) = 
        Cx(fn (t,f) => Tr.CJUMP(Tr.TEST(Tr.EQ, 
                                        F.externalCall("stringGreaterThanEqual",
                                                        [unEx(left), unEx(right)]),
                                        Tr.CONST (1)), t, f))

    | strCmp (left, A.GtOp, right) = 
        Cx(fn (t,f) => Tr.CJUMP(Tr.TEST(Tr.EQ, 
                                        F.externalCall("stringGreaterThan",
                                                        [unEx(left), unEx(right)]),
                                        Tr.CONST (1)), t, f))
    | strCmp (left, A.LeOp, right) = 
        Cx(fn (t,f) => Tr.CJUMP(Tr.TEST(Tr.EQ, 
                                        F.externalCall("stringLessThanEqual",
                                                        [unEx(left), unEx(right)]),
                                        Tr.CONST (1)), t, f))

    | strCmp (left, A.LtOp, right) = 
        Cx(fn (t,f) => Tr.CJUMP(Tr.TEST(Tr.EQ, 
                                        F.externalCall("stringLessThan",
                                                        [unEx(left), unEx(right)]),
                                        Tr.CONST (1)), t, f))
    | strCmp (left, A.EqOp, right) = 
        Cx(fn (t,f) => Tr.CJUMP(Tr.TEST(Tr.EQ, 
                                        F.externalCall("stringEqual",
                                                        [unEx(left), unEx(right)]),
                                        Tr.CONST (1)), t, f))
    | strCmp (left, A.NeqOp, right) = 
        Cx(fn (t,f) => Tr.CJUMP(Tr.TEST(Tr.EQ, 
                                        F.externalCall("stringEqual",
                                                        [unEx(left), unEx(right)]),
                                        Tr.CONST (0)), t, f))
    | strCmp(left, _, right) = (ErrorMsg.impossible "Unsupported string operation\n"; Ex(Tr.CONST 0))

  fun arthExp (left, A.PlusOp, right) = Ex(Tr.BINOP(Tr.PLUS, unEx(left), unEx(right)))
    | arthExp (left, A.MinusOp, right) = Ex(Tr.BINOP(Tr.MINUS, unEx(left), unEx(right)))
    | arthExp (left, A.TimesOp, right) = Ex(Tr.BINOP(Tr.MUL, unEx(left), unEx(right)))
    | arthExp (left, A.DivideOp, right) = Ex(Tr.BINOP(Tr.DIV, unEx(left), unEx(right)))
    | arthExp (left, _, right) = (ErrorMsg.impossible "Unsupported operation\n"; Ex(Tr.CONST 0))

  fun assignExp(mem, e) = Nx(Tr.MOVE(unEx(mem), unEx(e)))

  fun simpleVar (defineLvl as (LEVEL(parent, pu), offS), (* access *)
                 curLevel) =  (* current level *)
        Ex(Tr.MEM(Tr.BINOP(Tr.PLUS, 
                           Tr.CONST offS, 
                           getSLlocals(LEVEL(parent,pu), curLevel, Tr.TEMP (Register.FP))), 
           F.ws))
    | simpleVar((TOP,_),_) = (ErrorMsg.impossible "Function level does not exist.\n"; Ex(Tr.CONST 0))

  fun fieldVar (mem, i) =
    Ex(Tr.ESEQ(Tr.EXP(F.externalCall("recordNilCheck",[unEx mem])),
      (Tr.MEM(Tr.BINOP(Tr.PLUS,
                      unEx mem,
                      Tr.BINOP(Tr.MUL,unEx i,Tr.CONST F.ws)),
              F.ws))))   

  fun subsVar(mem, i) = Ex(Tr.ESEQ(Tr.EXP(F.externalCall("arrayBounds",[unEx mem,unEx i])),
                            Tr.MEM(Tr.BINOP(Tr.PLUS, 
                                            unEx(mem), 
                                            Tr.BINOP(Tr.MUL,
                                                     Tr.BINOP(Tr.PLUS,unEx(i),Tr.CONST 1),
                                                     Tr.CONST F.ws)), 
                                  F.ws)))

  fun formals(0) = []
    | formals(n) = (Register.paramBaseOffset+F.ws*(n))::formals(n-1) 
    (* is reversed in semant *)
    (* does not include static link because interact with semant *)

end (* functor TranslateGen *)





     
