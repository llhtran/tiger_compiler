(* regalloc.sml *)

signature REG_ALLOC =
sig
   structure R : REGISTER_STD
   
   type allocation = Register.register Temp.Table.table

   val color : {interference : Liveness.igraph,
                initial : allocation,
                registers : R.register list} -> allocation
   
   val regAlloc : (Assem.instr list * (Temp.temp * R.register) list * R.register list) ->
                    ((Assem.instr * Temp.temp list) list * allocation)  
   
   val regAlloc' : (Assem.instr list * allocation * R.register list) ->
                    ((Assem.instr * Temp.temp list) list * allocation)                 
   (* val regAlloc : (Assem.instr list * (Temp.temp * R.register) list * 
                     R.register list -> ((Assem.instr * Temp.temp list) list * allocation) *)
   
   (* 
     val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
     
     val interferenceGraph : 
        Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)
  
     ListPair.zip (Assem.instr list) (map f (Flow.Graph.node list))
   
     val color : {interference : Liveness.igraph,
                  initial : allocation,
                  registers : R.register list} -> allocation
  *)

end (* signature REG_ALLOC *)

functor RegAllocGen(Register : REGISTER_STD) : REG_ALLOC =
struct
   structure R : REGISTER_STD = Register

   type allocation = R.register Temp.Table.table

   (* The color function talkes an initial allocation table (which assigns
      temporary variables such as FP or SP into certain fixed registers)
      plus an interference graph and a list of registers, and returns
      a new allocation table (mapping from temporaries to registers).

      Notice, you don't need to implement spilling and coalescing. 
      Just do the "simplify" and then do the "select".
    *)
   structure M = MakeGraph
   structure L = Liveness
   structure G = Graph

   fun bug s = ErrorMsg.impossible ("RegAlloc:" ^ s)
    (* Do not kill precolored nodes that are specified in the initial 
       allocation table; maintain their edges and colors *)

   fun tempRegtoAlloc (pairList : (Temp.temp * R.register) list) : allocation = 
   foldl (fn ((tmp,reg), tab) => Temp.Table.enter (tab, tmp, reg)) 
     Temp.Table.empty pairList
    (*
    1. Search out nodes with degree < length regList
    2. Delete the node and its edges, recursively call on the new igraph
    3. Continue until the only remaining nodes are the precolored regs
    4. Add the deleted nodes and edges back, going back up the recursion
    5. Write a function that, given a node, returns the list of neighboring colors
        - That way, we know what colors we may choose that do not interfere 
    6. function to determine degree of a node
    *)
   structure Set = ListSetFn(struct type ord_key = R.register; 
                            val compare = String.compare; end)
   fun listToSet lst = Set.addList(Set.empty, lst)
   
   fun color {interference=interGr, initial=alloc, registers=regs} = 
   let
     val L.IGRAPH{graph=gr,tnode=tn,gtemp=gt,...} = interGr
     (* val nodeList = G.nodes gr  no *)
     val numColors = length regs 
     fun node2temp n = valOf (Graph.Table.look (gt, n))
     fun temp2node tmp = valOf (Temp.Table.look (tn, tmp))
    
     fun predefNode n = isSome (Temp.Table.look(alloc, node2temp n))
     
     fun degreeNode n = length (G.succ n)
     fun neighbors n = G.succ n
     
     fun mk_uedge n1 n2 = (G.mk_edge{from=n1,to=n2}; G.mk_edge{from=n2,to=n1})
     fun rm_uedge n1 n2 = (G.rm_edge {from=n1,to=n2}; G.rm_edge {from=n2,to=n1})
     fun rmNode n nodeList = (app (rm_uedge n) (G.succ n); 
                              List.filter (fn x => not (G.eq (n, x))) nodeList)
     
     fun findNode nodeList : G.node option = 
        List.find (fn n => degreeNode n < numColors andalso not (predefNode n)) nodeList
    
     fun getNeighborColors n allocT : R.register list = 
     map (fn x => valOf (Temp.Table.look(allocT, node2temp x))) (neighbors n)
    
     fun chooseColor n allocT = hd (Set.listItems 
        (Set.difference (listToSet regs, listToSet (getNeighborColors n allocT))))
     
     fun simplifySelect nodeList alloc : allocation =
     (case findNode nodeList of
       NONE => if length nodeList = 0 orelse List.all predefNode nodeList 
               then alloc 
               else bug "Graph is not K-colorable; not enough registers" 
       (* Either not k colorable, or empty, or only predefined nodes left 
        * Should only error out if not colorable, otherwise treat as base case
        *)
     | SOME n => 
       let
         val neighborsN = neighbors n
         val reducedNodeList = rmNode n nodeList (* Delete edges, rm node from list*)
         val newAlloc = simplifySelect reducedNodeList alloc 
         val _ = app (mk_uedge n) (neighborsN) (* Add back the edges *)
       in
         Temp.Table.enter(newAlloc, node2temp n, chooseColor n newAlloc)
       end)
   in
     simplifySelect (G.nodes gr) alloc
     (*alloc *)
   end

   (* fun color {interference, initial, registers} = (* ... *) initial *)

   fun regAlloc' (instrs, alloc, regs) = 
   let
     val (flowGr, flNodes) = M.instrs2graph instrs
     val (interGr, flNode2liveOuts) = L.interferenceGraph flowGr
     val alloc' = color {interference=interGr, initial=alloc, registers=regs}
   in
     (ListPair.zip (instrs, map flNode2liveOuts flNodes), alloc')
   end
   
   fun regAlloc (instrs, pList, regs) = regAlloc' (instrs, tempRegtoAlloc pList, regs)
   
end (* functor RegAllocGen *)
