(* liveness.sml *)
(* as7 
  Lien Tran BR 2016 *)

signature LIVENESS =
sig

  datatype igraph = 
      IGRAPH of {graph : Graph.graph,
                 tnode : Graph.node Temp.Table.table,
                 gtemp : Temp.temp Graph.Table.table,
                 moves : (Graph.node * Graph.node) list}

  
    val interferenceGraph : 
          Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)
  
    (*val show : outstream * igraph -> unit*)
  

end (* signature LIVENESS *)

structure Liveness : LIVENESS = 
struct

  structure G = Graph 
  structure GT = Graph.Table
  structure TT = Temp.Table 
  structure FGT = Flow.Graph.Table
  (* QUESTION: can we get rid of the semi-colon? *)
  structure ListSet = ListSetFn (struct type ord_key = Temp.temp; 
                                 val compare = Int.compare end)

  datatype igraph = 
      IGRAPH of {graph : Graph.graph,
                 tnode : Graph.node Temp.Table.table,
                 gtemp : Temp.temp Graph.Table.table,
                 moves : (Graph.node * Graph.node) list}

  (* To construct the interference graph, it is convenient to
     construct a liveness map at each node in the FlowGraph first.
     For each node in the flowgraph, i.e., for each assembly 
     instruction, we want to easily look up the set S of live 
     temporaries. 
   *)

  type liveSet = unit Temp.Table.table * Temp.temp list
  type livenessMap = liveSet Flow.Graph.Table.table

  (* function to initialize empty sets and tables for each node *)
  fun initLive (nodeLst, initSet, initTable) = 
    let (* n for node, t for table *)
      val liveIn = foldl (fn (n,t) => GT.enter(t, n, initSet)) initTable nodeLst
      val liveOut = foldl (fn (n,t) => GT.enter(t, n, initSet)) initTable nodeLst
    in (liveIn, liveOut)
    end

  (* returns (in or out) set of a node *)
  fun getSet (liveTable, node) = valOf (GT.look(liveTable, node))

  fun interferenceGraph(Flow.FGRAPH{control,def,use,ismove}) = 
    let val nodeLst = G.nodes(control)
      val (liveIn, liveOut) = initLive(nodeLst, ListSet.empty, GT.empty)
      
      (* liveness algorithm as described in figure 10.4 *)
      fun iterate(nil, liveIn, liveOut, check) = (liveIn, liveOut, check)
        | iterate(node::lst, liveIn, liveOut, check) = 
        let 
          val inn' = getSet(liveIn, node)
          val outn' = getSet(liveOut, node)
          val defn = ListSet.addList(ListSet.empty, getSet(def, node))
          val usen = ListSet.addList(ListSet.empty, getSet(use, node))
          val inn = ListSet.union(usen, ListSet.difference(outn', defn))

          val ins = map (fn n => getSet(liveIn, n)) (G.succ node)
          val outn = foldl ListSet.union ListSet.empty ins

          val liveIn' = GT.enter(liveIn, node, inn)
          val liveOut' = GT.enter(liveOut, node, outn)
          val check' = (ListSet.equal(inn',inn) andalso
                       ListSet.equal(outn', outn)) :: check
        in iterate(lst, liveIn', liveOut', check')
        end


      (* keeps iterating until in and out sets for each node do not change *)
      fun calcLiveness(nlst, liveIn, liveOut) = 
        let
          val (liveIn', liveOut', check) = iterate(nlst, liveIn, liveOut, nil)
          val continue =  List.exists (fn x => not x) check
        in
          if continue 
          then calcLiveness(nlst, liveIn', liveOut')
          else (liveIn', liveOut')
        end

      (* calculates liveness tables *)
      val (liveInFinal, liveOutFinal) = calcLiveness(nodeLst, liveIn, liveOut)

      (* liveSet = unit Temp.Table.table * temp list *)
      (* set up table that maps temps to units *)
      (* used to check temp membership *)
      (* set up list that correspond to temps *)
      (* list used to make unions more convenient *)
      fun makeLiveSet (node,liveIn,liveOut) =
        let
          val liveAll = ListSet.listItems(ListSet.union(
                                          getSet(liveIn, node),
                                          getSet(liveOut, node)))
          val liveTable = List.foldl (fn (temp, table) =>
                        TT.enter(table, temp,())) TT.empty liveAll
        in (liveTable,liveAll) (* this is a liveSet *)
        end

      (* liveMap = liveSet Flow.Graph.Table.table *)  
      (* set up table that maps node to liveSets *)
      fun makeLiveMap(nodeLst, liveIn, liveOut) = 
        let
          val liveMap = List.foldl (fn(n, table) => 
            FGT.enter(table, n, makeLiveSet(n, liveIn, liveOut))) 
            FGT.empty nodeLst
        in liveMap
        end 

      val liveMap = makeLiveMap(nodeLst,liveInFinal,liveOutFinal)      

  (* after constructing the livenessMap, it is quite easy to
     construct the interference graph, just scan each node in
     the Flow Graph, add interference edges properly ... 
   *)

      val igraph = G.newGraph() 
      val tnode = TT.empty  (* mapping from temps to graph nodes *)
      val gtemp = GT.empty  (* mapping from graph nodes to temps *)  

      (* get all live temps from liveMap given a node *)
      fun getTemps node = 
        case FGT.look(liveMap, node) of
          SOME (liveTable,liveAll) => liveAll
        | NONE => ErrorMsg.impossible "Cannot get temps from liveMap."

      (* Creating interference by inserting edges to tables *)
      (* for easy mapping betwen temps and nodes *)
      fun makeiGraph(tnode, gtemp, nil) = (tnode, gtemp)
        | makeiGraph(tnode, gtemp, node::lst) = 
          let
            val liveAll = getTemps node   (* get all live temporaries *)
            val defn = getSet(def, node)  (* get all defs for current node *)

            (* for every newly defined temporary d at inode *)
            (* creat a corresponding igraph node *)
            (* set up tables to connect that node to all other live temps *)
            fun makeNode(d, tnode, gtemp) =
              let
                val inode = G.newNode igraph
                val tnode' = TT.enter(tnode, d, inode)
                val gtemp' = GT.enter(gtemp, inode, d)
              in
                (inode, tnode',gtemp') 
              end

            fun checkNode(d, tnode, gtemp) = 
              case TT.look(tnode, d) of 
                NONE => makeNode(d, tnode, gtemp)
              | SOME inode => (inode, tnode, gtemp)

            (* adding edges to igraph connecting igraph nodes *)
            (* to other live temps at that node *)
            fun addEdges(d, nil, tnode, gtemp) = (tnode, gtemp)
              | addEdges(d, live::lst, tnode, gtemp) = 
                let
                  val (from, tfrom, gfrom) = checkNode(d, tnode, gtemp)
                  val (to, tto, gto) = checkNode(live, tfrom, gfrom)
                in
                  (G.mk_edge{from=from, to=to}; 
                   addEdges(d, lst, tto, gto))
                end

            (* iterate through all defs temporaries *)
            fun iterateDef(nil, (tnode, gtemp)) = (tnode, gtemp)
              | iterateDef(d::dLst, (tnode, gtemp)) = 
                  iterateDef(dLst, addEdges(d, liveAll, tnode, gtemp))

            (* final resulting mapping tables *)
            val (tnodeNext, gtempNext) = iterateDef(defn, (tnode, gtemp))

          in makeiGraph(tnodeNext, gtempNext, lst)
          end

        val (tnodeFinal, gtempFinal) = makeiGraph(tnode, gtemp, nodeLst)

    in (IGRAPH{graph=igraph,tnode=tnodeFinal,gtemp=gtempFinal,moves=nil}, getTemps)
    end (* NOTE & TODO: Not optimized for move instructions *)
end (* structure Liveness *)
     