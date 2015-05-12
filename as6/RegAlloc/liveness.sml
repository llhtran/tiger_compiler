(* liveness.sml *)

signature LIVENESS =
sig

  datatype igraph = 
      IGRAPH of {graph : Graph.graph,
                 tnode : Graph.node Temp.Table.table, (* temp -> node *)
                 gtemp : Temp.temp Graph.Table.table, (* node -> temp *)
                 moves : (Graph.node * Graph.node) list}

 (* Live out mapping  *) 
      val interferenceGraph : 
           Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)
    
      val show : TextIO.outstream * igraph -> unit

end (* signature LIVENESS *)

structure Liveness : LIVENESS = 
struct

  datatype igraph = 
      IGRAPH of {graph : Graph.graph,
                 tnode : Graph.node Temp.Table.table,
                 gtemp : Temp.temp Graph.Table.table,
                 moves : (Graph.node * Graph.node) list}
(* Nodes are now temporaries, moves represent tuples between temporaries, I think? *)

(* Liveness for nodes means 
 - If node uses the temp in the instruction
 - Or if the temp is live-out at the node
 - Actually, I only need live out 
 *)

  (* To construct the interference graph, it is convenient to
     construct a liveness map at each node in the FlowGraph first.
     For each node in the flowgraph, i.e., for each assembly 
     instruction, we want to easily look up the set S of live 
     temporaries. 
   *)
  structure G = Graph
  structure F = Flow
  structure M = MakeGraph


  type liveSet = unit Temp.Table.table * Temp.temp list (*Membership w/ temp key, enumerate set*)
  type livenessMap = liveSet Flow.Graph.Table.table (*Takes node keys*)
 
  structure Set = BinarySetFn(struct type ord_key = Temp.temp; 
                            val compare = Int.compare; end)
  fun listToSet lst = Set.addList(Set.empty, lst)
 
  (* Flow.flowgraph * Flow.Graph.node list *)
  
 (* 1. Iterate through nodes to update live in, live out until at fixed point
  * 2. Write function to update ONE node's live in and out sets; will need access
        to all live in/live out sets for all nodes in order to compute
  * 3. Succ and pred Graph calls will return nodes (g,int/node'), NOT ints
  * 4. How to store the intermediate live in/out sets? Map from node key to in/out sets
  * 5. Treat the table as an updateable, functional data structure
  * 6. Should check if update occurred incrementally, not all together
  *)

  (* Given node, returns tuple of live in, live out sets *)
  type liveTable = (Set.set * Set.set) Graph.Table.table
  
  (* fun initLiveTable nodeList : liveTable = 
    foldl (fn (n,g)=>G.Table.enter(g,n, (Set.empty,Set.empty))) G.Table.empty nodeList
  *)

  (* Accessors for livein and liveout sets for a given node in a given livetable
   * No checking for null mapping; assuming all searches will succeed
   *)  
  fun getLiveIn n (t : liveTable) = #1 (valOf (G.Table.look(t,n)))
  fun getLiveOut n (t : liveTable) = #2 (valOf (G.Table.look(t,n)))

  fun updateLiveNode tab node defT useT : (liveTable * bool) = 
  let 
    val (livein', liveout') = (getLiveIn node tab, getLiveOut node tab)
    val (SOME defs, SOME uses) = (G.Table.look(defT, node), G.Table.look(useT, node))
    val livein = Set.union(listToSet uses, Set.difference (liveout', listToSet defs))
    val liveout =
      foldl (fn (succ, set) => Set.union(set, getLiveIn succ tab)) Set.empty (G.succ node)
    (* function that iterates thru succs of node, and unions all
                    their livein sets *)
  in
    (G.Table.enter(tab, node, (livein,liveout)),
    Set.equal(livein',livein) andalso Set.equal(liveout',liveout)) (*DONE == true if equal*)
  end
    
  fun printLiveness liveT nodeList = 
  let
    fun liveInStr liveT n = String.concatWith " " 
        (map Temp.makestring (Set.listItems (getLiveIn n liveT)))
    fun liveOutStr liveT n = String.concatWith " "
        (map Temp.makestring (Set.listItems (getLiveOut n liveT)))
    fun liveStr liveT n = "In: " ^ (liveInStr liveT n) ^ " Out: " ^ (liveOutStr liveT n)
  in
    print (String.concatWith "\n" (map (liveStr liveT) nodeList) ^ "\n")
  end
  
  
  fun updateLiveTable liveT nodeList defT useT = 
  let 
    val (liveT', done) = foldl (fn (n, (lT, stop)) => 
      let val (lT', stop') = updateLiveNode lT n defT useT 
      in (lT', stop andalso stop')
      end) (liveT, true) nodeList
  in
    if done then (liveT', done) 
        else updateLiveTable liveT' nodeList defT useT
  end



(*  type liveSet = unit Temp.Table.table * Temp.temp list (*Membership w/ temp key, enumerate set*)
  type livenessMap = liveSet Flow.Graph.Table.table (*Takes node keys*)
*)  
  fun getLivenessMap nodeList defT useT : livenessMap =
  let
    val initLiveTable = foldl (fn (n,g)=>G.Table.enter(g,n, (Set.empty,Set.empty))) 
                        G.Table.empty nodeList
    val finalLiveTable = #1(updateLiveTable initLiveTable nodeList defT useT) 
    fun makeLiveSet node liveT = 
    let
      val tempList = Set.listItems (getLiveOut node liveT)
    in
      (foldl (fn (temp,t) => Temp.Table.enter(t,temp,()))
      Temp.Table.empty tempList, tempList)
    end

  in
    ((* print "done\n";printLiveness finalLiveTable nodeList; *)foldl (* fn that takes a node, and the liveSet table, and adds the liveset *) 
    (fn (node,t) => Flow.Graph.Table.enter(t,node,makeLiveSet node finalLiveTable))
    Flow.Graph.Table.empty nodeList)
  end

  (* after constructing the livenessMap, it is quite easy to
     construct the interference graph, just scan each node in
     the Flow Graph, add interference edges properly ... 
   *)

  val emptyIgraph  = IGRAPH {graph=G.newGraph(), tnode=Temp.Table.empty, 
    gtemp=G.Table.empty, moves=[]}

  (* fun nullF (n:F.Graph.node) : Temp.temp list = [] *)
(*  datatype igraph = 
      IGRAPH of {graph : Graph.graph,
                 tnode : Graph.node Temp.Table.table, (* temp -> node *)
                 gtemp : Temp.temp Graph.Table.table, (* node -> temp *)
                 moves : (Graph.node * Graph.node) list} *)

  fun interferenceGraph (F.FGRAPH {control=gf,def=d,use=u,ismove=mv}) = 
  let 
    val nodeList = G.nodes gf

    (* Fetches all temps ever found in use and def tables of all instrs *)
    val allTemps = Set.listItems (foldl (
        fn (node, set) => Set.union(set, Set.union (
        listToSet (valOf (G.Table.look(d,node))), 
        listToSet (valOf (G.Table.look(u,node)))))) 
        Set.empty nodeList)
    
    (* Adds the temp to graph, tnode, gtemp *)
    fun addTemp (temp, {graph=gr, tnode=tn,gtemp=gt}) = 
    let 
      val node = G.newNode gr
    in 
      {graph=gr, tnode=Temp.Table.enter(tn,temp,node), gtemp=G.Table.enter(gt,node,temp)}
    end

    (* Adds all temps to init the IGraph *)
    val {graph=gi, tnode=tn, gtemp=gt} = foldl addTemp 
      {graph=G.newGraph(), tnode=Temp.Table.empty, gtemp=G.Table.empty} allTemps
    
    val livenessMap = getLivenessMap nodeList d u
    
    (* Mapping instr flow nodes to liveout temps list *)
    fun fOut (n : Flow.Graph.node) : Temp.temp list = 
    let
    in
      #2 (valOf (Flow.Graph.Table.look (livenessMap, n)))
    end

    (* Making undirected edges the same as making cycle of directed edges *)
    fun mk_uedge (n1,n2) = (G.mk_edge{from=n1,to=n2}; G.mk_edge{from=n2,to=n1})
    (* fun mk_uedge (n1,n2) = (G.mk_edge{from=n1,to=n2}) *)
    fun isMoveNode node = valOf (G.Table.look(mv, node))

    fun procFlowNode node = 
    let 
      val (SOME defs, SOME uses) = (G.Table.look(d, node), G.Table.look(u,node))
      val liveouts = fOut node
      fun addInterEdge tmp1 tmp2 = 
      let val (SOME n1,SOME n2) = (Temp.Table.look(tn,tmp1),Temp.Table.look(tn,tmp2))
      in if G.eq (n1,n2) then () else mk_uedge (n1, n2)
      end
     
      (*fun appECross [] outs = (print ("Outs size = " ^ 
        (Int.toString (length outs))^ "\n"))
        | appECross (x::xs) outs = (app (addInterEdge x) outs; appECross xs outs)
       *)  
      fun appE outs def = app (addInterEdge def) outs
      fun appECross outs defs = app (appE outs) defs
    in
    (*if isMoveNode node then 
        appECross 
        (List.filter (fn x => not (List.exists (fn y => x = y) uses)) liveouts) defs
    else *) ((* print ((Int.toString (length liveouts)) ^ "\n"); *)
                appECross liveouts defs) 
    end
    (**) 
  in
    (app procFlowNode nodeList; 
    (IGRAPH{graph=gi,tnode=tn,gtemp=gt,moves=[]},fOut))
    (* (emptyIgraph, fOut) *)
  end
  
  fun show (outS, IGRAPH{graph=gi,tnode=tn,gtemp=gt,...}) =
  let
    val nodeList = G.nodes gi
    fun nodeToTempStr n = Temp.makestring (valOf (G.Table.look (gt, n)))
    fun printAdjTmp n = (nodeToTempStr n) ^ " : " ^ 
        (String.concatWith " " (map nodeToTempStr (G.succ n)))
  in
    TextIO.output(outS, (String.concatWith "\n" (map printAdjTmp nodeList)) ^ "\n")
  end 
(* Should add all temporaries to the graph before assigning edges?
   Or do it incrementally? *)
(*
  (* Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list) *)
  fun interferenceGraph fGraph = (emptyIgraph, nullF)
*)
end (* structure Liveness *)

     

                 
