(* makegraph.sml *)

(* Reference for Assem instrs; delete when done
signature ASSEM =
sig
  type reg = string
  type temp = Temp.temp
  type label = Temp.label

  datatype instr = OPER of {assem: string,
			    dst: temp list,
			    src: temp list,
			    jump: label list option} label is an alias for symbol
                 | LABEL of {assem: string, lab: Temp.label}
                 | MOVE of {assem: string, 
			    dst: temp,
			    src: temp}

  datatype flowgraph 
    = FGRAPH of {control: Graph.graph, Graph is a monomorphic dynamic array
                 def: Temp.temp list Graph.Table.table, map instrs (as nodes) -> temp list (regs, as ints)
                 use: Temp.temp list Graph.Table.table,
                 ismove: bool Graph.Table.table} map instrs (as nodes) -> bool
*)

signature MAKEGRAPH = 
sig
  val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end

structure MakeGraph : MAKEGRAPH =
struct

  structure A = Assem
  structure F = Flow
  structure G = Graph
  structure T = Graph.Table
  fun bug s = ErrorMsg.impossible ("MakeGraph:" ^ s)

  val newFlowGraph = F.FGRAPH {control = G.newGraph(),
                               def = G.Table.empty,
                               use = G.Table.empty,
                               ismove = G.Table.empty}
    
  (*
    1. Add all instrs as nodereps to the graph (no edges yet)
    2. Get a list of (label * node number) elmts
    2.5. Process the def/use temporaries
    3. Walk through all instrs a second time to build the edges, with the list 
       of tuples as a lookup mapping from labels to nodes 
  *)
  
  (* Given one assem instr,  *)
  fun procInstr (instr, (items as {ctrl = (c:G.graph),
                         def = (d:Temp.temp list T.table),
                         use = (u:Temp.temp list T.table),
                         ismove = (mv:bool T.table),
                         labels = (l:(Temp.label * G.node) list)})) =
  (* in all cases, add node to graph; also check if is move stm *)
  (*table key val*)
  let 
    val node = G.newNode c
    fun update n (dst,src,move,lab) {ctrl=c,def=d,use=u,ismove=mv,labels=l} = 
    (case lab of 
      NONE => {ctrl=c,def=T.enter(d,n,dst),use=T.enter(u,n,src),
        ismove=T.enter(mv,n,move),labels=l}
    | SOME x => {ctrl=c,def=T.enter(d,n,dst),use=T.enter(u,n,src),
        ismove=T.enter(mv,n,move),labels=(x,n)::l})
  in
    (case instr of
      (* add to use and def tables *)
      A.OPER {assem,dst,src,jump} => update node (dst,src,false,NONE) items
      (* add label to list of labels *)
    | A.LABEL {assem, lab} => update node ([],[],false,SOME lab) items
      (* add to ismove; add to use and def tables *)
    | A.MOVE {assem,dst,src} => update node ([dst],[src],true,NONE) items)
  end
 
  (* Given assembly instrs, returns a record of fields for flowgraph, except
  no edge formatting and with a list of labels to build edges next *)
  fun procInstrs instrLst = 
  let 
    val items = {ctrl=G.newGraph(), def=T.empty, use=T.empty, ismove=T.empty, 
    labels=[]}
  in
    foldl procInstr items instrLst
  end

  (* Given the label list mapping and instruction, adds edge as necessary *)
  fun addEdge instr (node:G.node) (next:G.node) (last:bool) 
    (l:(Temp.label * G.node) list) =
  let
    fun findLab lab [] = (bug ("Cannot find label: "^(Symbol.name lab)); node) (*error*)
      | findLab lab ((l,n)::xs) = if lab = l then n else findLab lab xs
    fun addLabList labLookup jumpLab = 
        G.mk_edge {from=node, to=(findLab jumpLab labLookup)}
  in
    case instr of 
    (*  A.OPER {assem,dst,src,jump=NONE} => if last then () 
        else G.mk_edge {from=node,to=next} *)
    (*|*) A.OPER {assem,dst,src,jump=SOME xs} => app (addLabList l) xs
    | _ => if last then () else G.mk_edge {from=node,to=next}
    (* | _ => () *)
  end

  (* Adds all edges between nodes in the graph *)
  fun addEdges g instrList lookup =
  let
    val instrNodes = ListPair.zip (instrList, G.nodes g)
    fun f [] = ()
      | f [(x,y)] = addEdge x y y true lookup
      | f ((x,y)::xs) = (addEdge x y (#2 (hd xs)) false lookup; f xs)
  in
    f instrNodes
  end

  (* Constructs the control flow graph and annotations *)
  fun instrs2graph instrList =
  let
    val {ctrl=c,def=d,use=u,ismove=mv,labels=l} = procInstrs instrList
    (* For debugging purposes *)
    val (labs,_) = ListPair.unzip l
    val _ = print "DEBUG: PRINTING ALL LABELS\n"
    val _ = app (fn x => print (x ^ "\n")) (map Symbol.name labs)
    val _ = print "DEBUG: DONE\n"
    (* END *)
  in
    (addEdges c instrList l; (F.FGRAPH{control=c,def=d,use=u,ismove=mv}, G.nodes c))
  end

  (*fun instrs2graph instrList = 
  let 
    val initGraph = newFlowGraph

    val F.FGRAPH {control = gr,...} = initGraph

    (* Pulls, captures all added nodes *)
    fun addNodes aLst g = map (fn (a:A.instr) => G.newNode g) aLst
  in (initGraph, G.nodes gr)
  end *)

(* The "instrs2graph" function takes a list of assembly instructions,
   and constructs its flowgraph and also returns the list of nodes in 
   the flowgraph. The instructions exactly correspond to the nodes in 
   the graph. If instruction m can be followed by instruction n (either
   by a jump or by falling through), there should be an edge from m to n
   in the graph.

   The flowgraph also maintains several attributes for each node in the 
   graph, i.e., the "def" set, the "use" set, and the "ismove" flag

 *)
   

end
