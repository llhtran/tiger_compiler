(* regalloc.sml *)
(* as7 Lien Tran BR 2016 *)

signature REG_ALLOC =
sig
   structure R : REGISTER_STD
   
   type allocation = Register.register Temp.Table.table

   val color : {interference : Liveness.igraph,
                initial : allocation,
                registers : R.register list} -> allocation

  val regAlloc : (Assem.instr list * (Temp.temp * R.register) list 
                 * R.register list) ->
                 ((Assem.instr * Temp.temp list) list * allocation)  

end (* signature REG_ALLOC *)

functor RegAllocGen(Register : REGISTER_STD) : REG_ALLOC =
struct
  structure R : REGISTER_STD = Register
  structure GT = Graph.Table
  structure TT = Temp.Table
  structure T = Temp
  structure L = Liveness
  structure G = Graph

  type allocation = R.register TT.table

  structure ListSet = ListSetFn (struct type ord_key = Temp.temp; 
                                 val compare = Int.compare end)

   (* The color function talkes an initial allocation table (which assigns
      temporary variables such as FP or SP into certain fixed registers)
      plus an interference graph and a list of registers, and returns
      a new allocation table (mapping from temporaries to registers).

      Notice, you don't need to implement spilling and coalescing. 
      Just do the "simplify" and then do the "select".
    *)

  fun color {interference as L.IGRAPH{graph=igr, tnode=tnode, gtemp=gtemp,...}, 
             initial, 
             registers} = 
    let
      val nodeLst = G.nodes(igr)
      val k = length registers
      (* maintains a table that keeps track of how many edges each node has *)
      (* use to subtract edges as algorithm simplifies graph *)
      val edgeTable = List.foldl (fn (n, table) => 
                      GT.enter(table, n, List.length (G.adj(n))))
                      GT.empty nodeLst
      fun numEdges (node, table) = valOf (GT.look(table, node))

      (* set up working lists for simplify nodes and spill nodes [not used] *)
      fun makeWorkLists (node, (simplifyWorkList,spillWorkList,table)) =
        if numEdges(node, table) >= k then
          (simplifyWorkList, spillWorkList @ [node], table)
        else 
          (simplifyWorkList @ [node],spillWorkList, table)

      (* use a stack for simplify nodes *)
      val stack = ref []

      val (simplifyWorkList,spillWorkList,table) = 
          List.foldl makeWorkLists (nil,nil,edgeTable) nodeLst

      fun simplify(nil, spillWorkList, edgeTable) = (nil, spillWorkList, edgeTable)
       |  simplify(node::simplifyWorkList, spillWorkList, edgeTable) =

        (stack := [node] @ (!stack);  
        let
          fun DecrementDegree(m,table) =
            GT.enter(table, m, numEdges(m,table) - 1)

          val edgeTable' = foldl (fn(m,table) => DecrementDegree(m,table))
                           edgeTable (G.adj node)

          val (simplifyWorkList',spillWorkList',table) =
               List.foldl makeWorkLists (simplifyWorkList,nil,edgeTable') spillWorkList                       

        in simplify(simplifyWorkList',spillWorkList',edgeTable')
        end)
      
      val (simplifyWorkList', spillWorkList', edgeTable') = 
          simplify(simplifyWorkList, spillWorkList, edgeTable)

      fun select (n, colorAlloc) =
        let
          (* Get the temporary for n *)
          val ntemp = (case GT.look(gtemp,n) of
                        SOME(t) => t
                      | NONE =>  ErrorMsg.impossible "interference graph incorrect")

          fun getColor(node,ntemp,colorAlloc) =
                let
                  val adjoins = G.adj(n)

                  fun usedColors(node,used) =
                    let
                      val nodetemp = (case GT.look(gtemp,node) of
                                    SOME(x) => x
                                 |  NONE =>  ErrorMsg.impossible "interference graph incorrect")
                      val used'= (case TT.look(colorAlloc,nodetemp) of
                                      SOME(x) => used @ [x]
                                   |  NONE => used)                     
                    in
                      used'
                    end
                  val used = List.foldl usedColors nil adjoins

                  (* pick the first available register to color with *)  
                  fun neq a b = a <> b
                  val freecolor =
                    hd (List.filter (fn x => List.all (neq x) used) registers)
                  in freecolor
                  end
          
          val ncolor = getColor(n,ntemp,colorAlloc)           

          val colorAlloc' = 
              case TT.look(colorAlloc,ntemp) of
                  SOME(x) => colorAlloc
               |  NONE => TT.enter(colorAlloc,ntemp,ncolor)
        in
          colorAlloc'
        end  
    in
      List.foldl select initial (!stack)
    end 

  (* set up allocation set for special registers *)
  fun enterRegs (specialRegs : (T.temp * R.register) list) : allocation = 
    foldl (fn ((tmp, reg), tab) => TT.enter(tab, tmp, reg)) TT.empty specialRegs

  fun regAlloc (instrs, specialRegs, availRegs) = 
    let
      val allocation = enterRegs specialRegs
      val (flowGraph, flowNodes) = MakeGraph.instrs2graph instrs
      val (iGraph, nodeToTempFn) = Liveness.interferenceGraph flowGraph
      val finalAlloc = color {interference=iGraph, 
                             initial=allocation, 
                             registers=availRegs}
    in
      (ListPair.zip (instrs, map nodeToTempFn flowNodes), finalAlloc)
    end

end (* functor RegAllocGen *)
