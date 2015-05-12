(* codegen.sml *)

signature CODEGEN =
sig
  structure F : FRAME
  structure R : REGISTER

  (* translate each canonical tree into a list of assembly instructions *)
  val codegen : Tree.stm -> Assem.instr list

  (* converting a string fragment into assembly code *)
  val string : Temp.label * string -> string

  (* procEntryExit sequence + function calling sequence tune-up
   * + mapping pseudo-registers to memory load/store instructions
   * and actual registers.
   * This is a post-pass, to be done after register allocation.
   *)
  val procEntryExit : {name : Temp.label,
                          body : (Assem.instr * Temp.temp list) list,
                          allocation : R.register Temp.Table.table,
                          formals : Temp.temp list,
                          frame : Frame.frame} -> Assem.instr list

  val genSpills : (Assem.instr list * (Temp.temp -> string)) -> Assem.instr list

end (* signature CODEGEN *)

structure Codegen : CODEGEN =
struct

  structure T = Tree
  structure A = Assem
  structure Er = ErrorMsg
  structure F = Frame
  structure R = Register
  structure S = Symbol

  fun codegen (x) =
    let
      val ilist = ref (nil : A.instr list)

      fun emit y =
            ilist := (y :: !ilist)
      and result(gen) =
            let val t = Temp.newtemp() in (gen(t); t) end

      and constToString(n) =
            if (n < 0) then
              "$-" ^ Int.toString(n * ~1)
            else
              "$" ^ Int.toString(n)

      and saveRegs(len) = (
            emit(A.OPER{assem="movl %ecx, " ^ Int.toString(4 * len + 4) ^ "(%esp)\n", src=[], dst=[], jump=NONE});
            emit(A.OPER{assem="movl %edx, " ^ Int.toString(4 * len) ^ "(%esp)\n", src=[], dst=[], jump=NONE})
          )

      and restoreRegs(len) = (
            emit(A.OPER{assem="movl " ^ Int.toString(4 * len + 4) ^ "(%esp), %ecx\n", src=[], dst=[], jump=NONE});
            emit(A.OPER{assem="movl " ^ Int.toString(4 * len) ^ "(%esp), %edx\n", src=[], dst=[], jump=NONE})
          )

      and relopToString(relop) =
            case relop
              of T.EQ => "je"
               | T.NE => "jne"
               | T.LT => "jl"
               | T.GT => "jg"
               | T.LE => "jle"
               | T.GE => "jge"
               (*| ULT => ""
               | ULE =>
               | UGT =>
               | UGE =>*)
               | _ => (
                  print("Unimplemented jump condition!");
                  "error"
               )
      and munchArgs(arg::args, i) = (
            emit(A.OPER{assem="movl `s0, " ^ Int.toString(4 * i) ^ "(%esp)\n", dst=[], src=[munchExp(arg)], jump=NONE});
            munchArgs(args, i + 1)
          )
        | munchArgs(nil, _) =
            ()
      and munchStm(T.MOVE(T.MEM(e1, size1), T.MEM(e2, size2))) =
            let
              val munch1 = munchExp(e1)
              val munch2 = munchExp(e2)
              val temp = Temp.newtemp()
            in
              print("Tiling a T.MOVE(T.MEM(e1, size1), T.MEM(e2, size2))\n");
              emit(A.OPER{assem="movl (`s0), `d0\n", dst=[temp], src=[munch2], jump=NONE});
              emit(A.OPER{assem="movl `s0, (`d0)\n", dst=[munch1], src=[temp], jump=NONE})
            end
        | munchStm(T.MOVE(T.MEM(T.CONST(const), size), e)) = (
            print("Tiling a T.MEM(T.CONST(const), size)\n");
            emit(A.OPER{assem="movl `s0, (" ^ constToString(const) ^ ")\n", dst=[], src=[munchExp(e)], jump=NONE})
          )
        | munchStm(T.MOVE(T.MEM(e1, size), e2)) = (
            print("Tiling a T.MOVE(T.MEM(e1, size), e2)\n");
            emit(A.OPER{assem="movl `s1, (`s0)\n", dst=[], src=[munchExp(e1), munchExp(e2)], jump=NONE})
          )
        | munchStm(T.MOVE(T.TEMP(t), T.CALL(a))) = (
            print("Tiling a T.MOVE(T.TEMP(t), T.CALL(e, args))\n");
            emit(A.OPER{assem="movl `s0, `d0\n", src=[munchExp(T.CALL(a))], dst=[t], jump=NONE})
          )
        | munchStm(T.MOVE(T.TEMP(t), e)) = (
            print("Tiling a T.MOVE(T.TEMP(t), e)\n");
            emit(A.OPER{assem="movl `s0, `d0\n", src=[munchExp(e)], dst=[t], jump=NONE})
          )
        | munchStm(T.EXP(T.CALL(a))) = (
            munchExp(T.CALL(a));
            ()
          )
        | munchStm(T.CJUMP(T.TEST(relop, e1, e2), lab1, lab2)) = (
            print("Tiling a CJUMP(T.TEST(relop, e1, e2), branch1, branch2)");
            emit(A.OPER{assem="cmp `s0, `s1\n", src=[munchExp(e1), munchExp(e2)], dst=[], jump=NONE});
            emit(A.OPER{assem=relopToString(relop) ^ " " ^ S.name(lab1) ^ "\n", src=[], dst=[], jump=SOME([lab1, lab2])})
          )
        | munchStm(T.LABEL(label)) = (
            print("Tiling a T.LABEL(label))\n");
            emit(A.LABEL{assem=S.name(label) ^ ":\n", lab=label})
          )
        | munchStm(T.JUMP(T.NAME(label), lablist)) = (
            print("Tiling a T.JUMP(exp, lablist)\n");
            emit(A.OPER{assem="jmp " ^ S.name(label) ^ "\n", dst=[], src=[], jump=SOME(lablist)})
          )
        | munchStm(T.EXP(e)) = (
            print("Tiling a T.EXP(e)\n");
            munchExp(e);
            ()
        )
        | munchStm(tree) = (
            print("munchStm: Compiler error! No match for IR tree:\n");
            Printtree.printtree(TextIO.stdOut, tree)
          )

      and munchExp(T.BINOP(T.PLUS, T.CONST(const), e)) =
            result(fn r => (
              print("Tiling a T.BINOP(T.PLUS, T.CONST(const), e)\n");
              emit(A.OPER{assem="movl " ^ constToString(const) ^ ", `d0\n", src=[], dst=[r], jump=NONE});
              emit(A.OPER{assem="addl `s0, `d0\n", src=[munchExp(e), r], dst=[r], jump=NONE})
            ))
        | munchExp(T.BINOP(T.PLUS, e, T.CONST(const))) =
            result(fn r => (
              print("Tiling a T.BINOP(T.PLUS, e, T.CONST(const))\n");
              emit(A.OPER{assem="movl `s0, `d0\n", src=[munchExp(e)], dst=[r], jump=NONE});
              emit(A.OPER{assem="addl " ^ constToString(const) ^ ", `d0\n", src=[], dst=[r], jump=NONE})
            ))
        | munchExp(T.BINOP(T.PLUS, e1, e2)) =
            result(fn r => (
              print("Tiling a T.BINOP(T.PLUS, e1, e2)\n");
              emit(A.OPER{assem="movl `s0, `d0\n", src=[munchExp(e1)], dst=[r], jump=NONE});
              emit(A.OPER{assem="addl `s0, `d0\n", src=[munchExp(e2), r], dst=[r], jump=NONE})
            ))
        | munchExp(T.BINOP(T.MINUS, T.CONST(const), e)) =
            result(fn r => (
              print("Tiling a T.BINOP(T.MINUS, T.CONST(const), e)\n");
              emit(A.OPER{assem="movl " ^ constToString(const) ^ ", `d0\n", src=[], dst=[r], jump=NONE});
              emit(A.OPER{assem="subl `s0, `d0\n", src=[munchExp(e), r], dst=[r], jump=NONE})
            ))
        | munchExp(T.BINOP(T.MINUS, e, T.CONST(const))) =
            result(fn r => (
              print("Tiling a T.BINOP(T.MINUS, e, T.CONST(const))\n");
              emit(A.OPER{assem="movl `s0, `d0\n", src=[munchExp(e)], dst=[r], jump=NONE});
              emit(A.OPER{assem="subl $" ^ constToString(const) ^ ", `d0\n", src=[], dst=[r], jump=NONE})
            ))
        | munchExp(T.BINOP(T.MINUS, e1, e2)) =
            result(fn r => (
              print("Tiling a T.BINOP(T.MINUS, e1, e2)\n");
              emit(A.OPER{assem="movl `s0, `d0\n", src=[munchExp(e1)], dst=[r], jump=NONE});
              emit(A.OPER{assem="subl `s0, `d0\n", src=[munchExp(e2), r], dst=[r], jump=NONE})
            ))
        | munchExp(T.BINOP(T.MUL, e1, e2)) =
            result(fn r => (
              print("Tiling a T.BINOP(T.MUL, e1, e2)\n");
              (*Move to eax*)
              emit(A.OPER{assem="movl `s0, `d0\n", src=[munchExp(e1)], dst=[R.RV], jump=NONE});
              emit(A.OPER{assem="mul `s0\n", src=[munchExp(e2), R.RV], dst=[R.RV], jump=NONE});
              emit(A.OPER{assem="movl `s0, `d0\n", src=[R.RV], dst=[r], jump=NONE})
            ))
        | munchExp(T.BINOP(T.DIV, e1, e2)) = 
            result(fn r => (
              print("Tiling a T.BINOP(T.DIV, e1, e2)\n");
              (*Move to eax*)
              emit(A.OPER{assem="movl `s0, `d0\n", src=[munchExp(e1)], dst=[R.RV], jump=NONE});
              emit(A.OPER{assem="div `s0\n", src=[munchExp(e1), R.RV], dst=[R.RV], jump=NONE});
              emit(A.OPER{assem="movl `s0, `d0\n", src=[R.RV], dst=[r], jump=NONE})
            ))
        | munchExp(T.CALL(T.NAME fName, args)) = (
              print("Tiling a T.CALL(T.NAME fname, args)");
              saveRegs(length(args));
              munchArgs(rev(args), 0);
              emit(A.OPER{assem="call " ^ Symbol.name(fName) ^ "\n", src=[], dst = [R.RV], jump=NONE});
              restoreRegs(length(args));
              R.RV
          )
        | munchExp(T.MEM(e1, size)) =
            result(fn r => (
              print("Tiling a T.MEM(e1, size)\n");
              emit(A.OPER{assem="movl (`s0), `d0\n", src=[munchExp(e1)], dst=[r], jump=NONE})
            ))
        | munchExp(T.CONST(const)) =
            result(fn r => (
              print("Tiling T.CONST(" ^ constToString(const) ^ ")\n");
              emit(A.OPER{assem="movl " ^ constToString(const) ^ ", `d0\n", src=[], dst=[r], jump=NONE})
            ))
        | munchExp(T.TEMP(t)) = (
            print("Tiling a T.TEMP(t)\n");
            t
          )
        | munchExp(T.NAME(name)) = (
            result(fn r => (
              print("Tiling a T.NAME(" ^ Symbol.name(name) ^ ")\n");
              emit(A.OPER{assem="movl $(" ^ Symbol.name(name) ^ "), `d0\n", src=[], dst=[r], jump=NONE})
            ))
          )
        | munchExp(tree) = (
            print("munchExp: Compiler error! No match for IR tree (Inside exp):\n");
            Printtree.printtree(TextIO.stdOut, T.EXP(tree));
            Temp.newtemp()
          )
    in (
      (*Printtree.printtree(TextIO.stdOut, x);*)
      munchStm(x);
      rev(!ilist)
    )
    end

  fun string(lab, s) =
        Symbol.name(lab) ^ ":\n\t.int " ^ Int.toString(size(s)) ^ "\n\t.string \"" ^ s ^ "\"\n"

(*
  The following is an example implementation of mapping pseudo-registers
  to memory load/store instructions and actual registers.  It is done
  in a single pass.  It assumes that pseudo-register names start with
  the letter "f".  It uses the actual registers ECX and EDX as temporaries
  when a pseudo-register is an operand of an instruction.

  There is a special case that this function does NOT handle, but you MUST!
  The DIV instruction has special requirements.  Its dividend must be in EAX,
  its divisor in a general-purpose register.  It returns both the quotient,
  in EAX, and the remainder, in EDX regardless where the original divisor was!
  So be careful that a divide instruction does not trash something useful
  in EDX, and that you retrieve the correct result from the divide instruction.
*)

  (* regname -- produce an assembly language name for the given machine
   * register or psuedo-register.
   * psuedo-registers are mapped to an expression for psuedo-register's
   * location in stack frame.
   *)
  (* regname : R.register -> string *)
  fun regname reg =
    if (String.isPrefix "f" reg) then
  	  (* it's a psuedo-register *)
  	  let
  	      val (SOME prNum) = Int.fromString (String.extract(reg,1,NONE));
  	      val offset = (prNum + 1) * 4
  	  in
  	      "-" ^ Int.toString(offset) ^ "(%ebp)"
  	  end
    else
      reg


(* genSpills -- do our "poor man's spilling".  Maps all pseudo-register
 * references to actual registers, by inserting instructions to load/store
 * the pseudo-register to/from a real register
 *)
  fun genSpills (insns, saytemp) =
    let
  	  (* doload() -- given name of a source register src, and a true
  	   * machine register mreg, will return a load instruction (if needed)
  	   * and a machine register.
  	   *)
  	  (* loadit: Temp.temp * Temp.temp -> string * Temp.temp *)
  	  fun loadit (src,mreg) =
            let
  		        val srcnm = (saytemp src)
            in
  		        if (String.isPrefix "f" srcnm) then
  		          (* it's a fake register: *)
  		          let
          			  val _ = print ("loadit(): mapping pseudo-register `" ^ srcnm ^ "' to machine reg. `" ^ (saytemp mreg) ^"'\n");
          			  val loadInsn = "\tmovl\t" ^ (regname srcnm) ^ ", " ^ (saytemp mreg) ^ " # load pseudo-register\n"
  		          in
                  (loadInsn, mreg)
  		          end
  		        else
                (* no mapping needed *)
                ("", src)
      	      end
  	  (* mapsrcs : produce a sequence of instructions to load
  	   * pseudo-registers into real registers, and produce a list
  	   * of sources which reflects the real registers.
  	   *)
  	  (* mapsrcs : Temp.temp list * Temp.temp list -> string * Temp.temp list *)
  	  fun mapsrcs ([],_) = ("",[])
  	    | mapsrcs (src::srcs,mreg::mregs) =
            let
                val (loadInsn, src') = loadit(src,mreg)
                val (loadRest, srcs') = mapsrcs(srcs,mregs)
            in
                (loadInsn ^ loadRest, src'::srcs')
            end
  	  (* findit -- like List.find, but returns SOME i, where i is index
  	   * of element, if found
  	   *)
      fun findit f l =
        let
    		  fun dosrch([],f,_) = NONE
    		    | dosrch(el::els,f,idx) =
    		      if f(el) then
    			  SOME idx
    		      else
    			  dosrch(els,f,idx+1)
        in
  	      dosrch(l,f,0)
        end

  	  (* mapdsts -- after we have mapped sources to real machine
  	   * registers, iterate through dsts.
  	   * If dst is a pseudo-register then
  	   *    if dst was also a src,
  	   *         replace dst with machine register to which src is already
  	   *         mapped
  	   *    else
  	   *         map dst to its own machine register (just use %ecx)
  	   *    generate a store insn for dst to save result
  	   *)
      (* mapdsts : Temp.temp list * Temp.temp list * Temp.temp list ->
  	   *           string * Temp.temp list
  	   *)
      (* N.B.!  This only works for dst of length 0 or 1 !! *)
      (* pre: length(dsts) <= 1 *)
  	  fun mapdsts([],_,_) =
            ("",[])
  	    | mapdsts(dst::dsts,srcs,newsrcs) =
      	    let
        		  val found = findit (fn e => e=dst) srcs
        		  val dstnm = (saytemp dst)
            in
        		  if (isSome(found)) then
        		      (* this dst is also a source *)
        		      let
            			  val idx=valOf(found)
            			  val src=List.nth (srcs,idx)
            			  val mreg=List.nth (newsrcs,idx)
        		      in
            			  if (src <> mreg) then
            			      ("\tmovl\t`d0, " ^ (regname dstnm) ^ " # save pseudo-register\n", mreg::dsts)
            			  else
            			      (* no mapping *)
                    ("", dst::dsts)
      		        end
        		  else
        		    (* this dst isn't a source, but it might be a pseudo-register *)
                if (String.isPrefix "f" dstnm) then
                  (* it's a fake register: *)
                  (* we can safely just replace this destination with
                   * %ecx, and then write out %ecx to the pseudo-register
                   * location.  Even if %ecx was used to hold a different
                   * source pseudo-register, we won't end up clobbering
                   * it until after the source has been used...
                   *)
                  ("\tmovl\t`d0, " ^ (regname dstnm) ^ " # save pseudo-register\n", R.ECX::dsts)
                else
                  (* no mapping *)
                  ("", dst::dsts)
            end
  	  fun mapInstr(A.OPER{assem=insn, dst=dsts, src=srcs, jump=jmp}) =
            let
              val (loadinsns, newsrcs) = mapsrcs(srcs, [R.ECX, R.EDX]);
              val (storeinsns, newdsts) = mapdsts(dsts, srcs, newsrcs);
            in
        		  A.OPER{assem=loadinsns ^ insn ^ storeinsns,
                      dst=newdsts, src=newsrcs, jump=jmp}
            end
        | mapInstr(instr as A.LABEL _) =
            instr
        | mapInstr(instr) =
  	       (* we never generate these! *)
            ErrorMsg.impossible ("CodeGen: unexpected instruction type in mapInstr!")
    in
      map mapInstr insns
    end


    (* procEntryExit sequence + function calling sequence tune-up
   * + mapping pseudo-registers to memory load/store instructions
   * and actual registers.
   * This is a post-pass, to be done after register allocation.
   *)

  fun procEntryExit({name, body, allocation, formals, frame}) =
        let
          val {numFormals, offlst, locals, maxargs} = frame
          (*Allocate space for call*)
          val _ = print("locals: " ^ Int.toString(!locals) ^ " maxargs: " ^ Int.toString(!maxargs) ^ "\n")
          val prologue = [
            A.LABEL{assem=Symbol.name(name) ^ ": \n", lab=name},
            A.OPER{assem="\tpushl `s0\n", src=[R.FP], dst=[], jump=NONE},
            A.OPER{assem="\tmovl `s0, `d0\n", src=[R.SP], dst=[R.FP], jump=NONE},
            A.OPER{assem="\tsubl $" ^ Int.toString(F.wordSize * (!locals + !maxargs + length(R.truecallersaves))) ^ ", `d0\n", src=[], dst=[R.SP], jump=NONE}
          ]
          val epilogue = [
            A.OPER{assem="\tmovl `s0, `d0\n", src=[R.FP], dst=[R.SP], jump=NONE},
            A.OPER{assem="\tpopl `d0\n", src=[], dst=[R.FP], jump=NONE},
            A.OPER{assem="\tret\n", dst=[], src=[], jump=NONE}
          ]
          fun getInstrs((A.OPER{assem, src, dst, jump},_)::xs) =
                A.OPER{assem="\t" ^ assem, src=src, dst=dst, jump=jump}::getInstrs(xs)
            | getInstrs((A.MOVE{assem, src, dst},_)::xs) =
                A.MOVE{assem="\t" ^ assem, src=src, dst=dst}::getInstrs(xs)
            | getInstrs((x,_)::xs) =
                x::getInstrs(xs)
            | getInstrs(nil) = nil
          val newBody = genSpills(getInstrs(body), (fn t => R.regToString(t, allocation)))
        in (
           prologue @ newBody @ epilogue
        )
        end
end (* structure Codegen *)
