(* makegraph.sml *)
(* as7 Lien Tran BR 2016 *)

signature MAKEGRAPH = 
sig
 
	val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list

end

structure MakeGraph : MAKEGRAPH =
struct

	structure G = Graph
	structure A = Assem
	structure T = G.Table 

(* The "instrs2graph" function takes a list of assembly instructions,
   and constructs its flowgraph and also returns the list of nodes in 
   the flowgraph. The instructions exactly correspond to the nodes in 
   the graph. If instruction m can be followed by instruction n (either
   by a jump or by falling through), there should be an edge from m to n
   in the graph.

   The flowgraph also maintains several attributes for each node in the 
   graph, i.e., the "def" set, the "use" set, and the "ismove" flag

 *)

	fun instrs2graph(instrlist : Assem.instr list) =
		let val g = G.newGraph()

			(* function to add nodes to graph *)
			fun addNode (instr, (def, use, ismove, nodes)) = 
				let
					val newnode = G.newNode g
					val (dst, src, move) =
						(case instr of 
	          	A.OPER({assem, dst, src, jump}) => (dst, src, false)
	          |	A.LABEL({assem, lab}) => (nil, nil, false)
	          | A.MOVE({assem, dst, src}) => ([dst], [src], true))
	        val def' = T.enter(def, newnode, dst) 
	        val use' = T.enter(use, newnode, src)
	        val move' = T.enter(ismove, newnode, move)
				in
					(def', use', move', [newnode] @ nodes)
				end
			val (def, use, ismove, nodes) = foldr addNode (T.empty,T.empty,T.empty,nil) instrlist
			val pairNodeInsLst = ListPair.zip(nodes, instrlist)

			(* add edges between nodes that connect through jumps *)
			fun addJump (node, instr as A.OPER{jump = SOME(labels),...}) = 
				let
					fun getNode (nil) _ = ErrorMsg.impossible "REGALLOC ERROR: Cannot find label."
						| getNode ((jnode,jinstr)::lst) label = 
						(case jinstr of
							A.LABEL{assem,lab} => 
								if label = lab then G.mk_edge({from=node,to=jnode})
								else getNode(lst) label
						| _ => getNode(lst) label)
				in
					map (getNode(pairNodeInsLst)) labels
				end
			| addJump (node, instr) = nil

			(* function to add edges, calls addJump *)
			fun addEdge ((node, instr as A.OPER{jump = SOME(lab),...})::lst) = 
				((addJump(node,instr)); addEdge(lst))
			| addEdge((node,instr)::nil) = ()
			| addEdge ((node,instr)::lst) = 
				(G.mk_edge({from=node, to=(#1(hd(lst)))}); addEdge(lst))
			| addEdge _ = ()
		in
			(addEdge(pairNodeInsLst);
			(Flow.FGRAPH{control=g, def=def, use=use, ismove=ismove},
			nodes))
		end
end