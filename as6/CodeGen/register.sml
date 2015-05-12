(* register.sml *)

signature REGISTER =
sig
  include REGISTER_STD

  val ECX : Temp.temp
  val EDX : Temp.temp

 (* we maintain a separate list here of true callersaves, so that
  * CodeGen will not emit code to "save" the pseudo-registers, since
  * they already live on the stack.
  *)
  val truecallersaves : register list (* CodeGen use only! *)

  (* number of pseudo-registers: *)
  val NPSEUDOREGS : int  (* CodeGen use only! *)

  (* if you like, you can add other stuff here *)

  val regToString : Temp.temp * register Temp.Table.table -> register
  val printSpecRegs : (Temp.temp * register) list -> unit

end (* signature REGISTER *)


structure Register : REGISTER =
struct

  type register = string

  val RV = Temp.newtemp()
  val FP = Temp.newtemp()

  val SP = Temp.newtemp()

  val ECX = Temp.newtemp()
  val EDX = Temp.newtemp()

  (* of course, none of the following should be empty list *)

  val NPSEUDOREGS = 10 (* change this to the proper value *)
  val localsBaseOffset : int = ~4 - (NPSEUDOREGS * 4) (* change this to the proper value *)
  val paramBaseOffset : int = 8  (* change this to the proper value *)

  val specialregs : (Temp.temp * register) list = [
    (RV, "%eax"),
    (FP, "%ebp"),
    (SP, "%esp"),
    (ECX, "%ecx"),
    (EDX, "%edx")
  ]

  val argregs : (Temp.temp * register) list = []
  val calleesaves : register list = []
  val truecallersaves : register list = ["%ecx", "%edx"]
  val callersaves : register list = ["%ecx", "%edx", "f0", "f1", "f2",
        "f3", "f4", "f5", "f6", "f7", "f8", "f9"] (* PLUS PSEUDOREGISTERS *)

  fun printSpecRegs((headTemp, headReg)::regList) = (
        print(headReg ^ " is: t" ^ Int.toString(headTemp) ^ "\n");
        printSpecRegs(regList)
      )
    | printSpecRegs(nil) =
        ()

  fun regToString(temp, alloc) =
        case Temp.Table.look(alloc, temp) of
            SOME(name) => name
          | NONE => "t" ^ Temp.makestring(temp)

end (* structure Register *)
