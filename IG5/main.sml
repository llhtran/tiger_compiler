(* main.sml *)

signature MAIN = 
sig 
  val compile : string -> unit
end (* signature MAIN *)

structure Main : MAIN = 
struct
  structure Semant = SemantGen(Register)
  structure RegAlloc = RegAllocGen(Register)

  structure C = Codegen
  structure F = C.F
  structure T = Temp

  fun emitproc out (F.DATA {lab, s}) = TextIO.output(out,C.string(lab,s))

    | emitproc out (F.PROC{name, body, frame}) =
        let (* val _ = print ("Emit " ^ name ^ "\n") *)
             (* val _ = Printtree.printtree(out,body); *) 

            val stms = Canon.linearize body
            val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
            val instrs' = List.concat(map C.codegen stms') 

            val (instrs_live,allocation) = RegAlloc.regAlloc(instrs',Register.specialregs,Register.availregs)

            (* 
             * Once the RegAlloc module is ready, you can get 
             * (1) a new list of body instrs together with its live 
             *     temporaries: (Assem.instr * Temp.temp list) list
             *
             * (2) a register allocation table CALLED "allocation"
             *
             * These information then can be fed into the C.procEntryExit
             * function to generate the proper function calling sequence,
             * procedure entry/exit sequences etc.
             * want to do C.procEntryExit before running format with the table    
             *)

             val instrs = C.procEntryExit({name=name,body=instrs_live,
              allocation=allocation,frame=frame})

            val format0 = Assem.format (fn t => (valOf(T.Table.look(allocation,t))))

         in app (fn i => TextIO.output(out,format0 i)) instrs
        end

  fun withOpenFile fname f = 
        let val out = TextIO.openOut fname
         in (f out before TextIO.closeOut out) 
               handle e => (TextIO.closeOut out; raise e)
        end 

  fun compile filename = 
        let val frags = Semant.transprog(Parse.parse filename)
         in withOpenFile (filename ^ ".s") 
                 (fn out => (app (emitproc out) frags))
        end

end (* structure Main *)



