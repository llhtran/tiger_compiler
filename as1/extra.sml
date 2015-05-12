(* CPSC 421 Compilers and Interpreters Spring 2015*)
(* Student: Lien Tran BR 2016 *)
(* Assignment #1 due January 28th 2015 *)
(* Extra credit *)

type key = string
(*datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF*)

(*fun insert(key,LEAF) = TREE(LEAF,key,LEAF)
|	insert(key,TREE(l,k,r)) = 
		if key<k then TREE(insert(key,l),k,r)
		else if key>k then TREE(l,k,insert(key,r))
		else TREE(l,key,r);*)

(* a *)
(* member function that returns true if item
	is found, and false otherwise. *)
fun member(key,LEAF) = false 
| 	member(key,TREE(l,k,r)) = 
		if key<k then member(key,l)
		else if key>k then member(key,r)
		else true;

(* test cases for part a *)
(*val myTree = LEAF;
val tree1 = insert("hi", myTree);
val tree2 = insert("cocktails", tree1);
val tree3 = insert("oranges", tree2);
val tree4 = insert("bye", tree3);

member("hi", tree4);
member("what", tree4);*)

(* b *)
(* Extension of the previous data structure and functions
	for handling binary trees, supports key bindings to a'.
	Names of previous data structure has "ex" added to 
	indicate extension, while at the same time allow the
	code for both to run when this file is executed. *)
datatype 'a extree = EXLEAF | EXTREE of 'a extree * key * 'a * 'a extree;

fun insert(EXLEAF,key,value) = EXTREE(EXLEAF,key,value,EXLEAF)
|	insert(EXTREE(l,k,v,r),key,value) = 
		if key<k then EXTREE(insert(l,key,value),k,v,r)
		else if key>k then EXTREE(l,k,v,insert(r,key,value))
		else EXTREE(l,key,value,r);

fun lookup(EXTREE(l,k,v,r),key) = 
		if key<k then lookup(l,key)
		else if key>k then lookup(r,key)
		else SOME v
|	lookup(EXLEAF,key) = NONE;

(* test cases for part b *)
(*val myTree = EXLEAF;
val tree1 = insert(myTree, "hi", 3);
val tree2 = insert(tree1, "cocktails", 10);
val tree3 = insert(tree2, "oranges", 1);
val tree4 = insert(tree3, "bye", 0);

lookup(tree4, "hi");
lookup(tree4, "what");*)

(* c *)
(* 		Part a: t s p i p f b s t 

	Inserting these keys into the tree data structure
	that we created will result in a tree unbalanced to the left.
	There will be no duplicates, because the data structure's
	operations inherently eliminate duplicates. As a consequence,
	not all insertions result in the addition of a node. A rough
	representaiton of the tree is shown, followed by functions
	to create the data structure (uncomment to run the code). 

		Resulting tree

			  t
			 /
			s
		   / 
		  p	  
	     /
	    i
	   /
	  f
     /
    b

CODE
val myTree1 = LEAF;
insert("t", myTree1);
insert("s", it);
insert("p", it);
insert("i", it);
insert("p", it);
insert("f", it);
insert("b", it);
insert("s", it);
insert("t", it);

RESULT
val myTree1 = LEAF : tree
val it = TREE (LEAF,"t",LEAF) : tree
val it = TREE (TREE (LEAF,"s",LEAF),"t",LEAF) : tree
val it = TREE (TREE (TREE #,"s",LEAF),"t",LEAF) : tree
val it = TREE (TREE (TREE #,"s",LEAF),"t",LEAF) : tree
val it = TREE (TREE (TREE #,"s",LEAF),"t",LEAF) : tree
val it = TREE (TREE (TREE #,"s",LEAF),"t",LEAF) : tree
val it = TREE (TREE (TREE #,"s",LEAF),"t",LEAF) : tree
val it = TREE (TREE (TREE #,"s",LEAF),"t",LEAF) : tree
val it = TREE (TREE (TREE #,"s",LEAF),"t",LEAF) : tree

*)

(*
		Part b: a b c d e f g h i

	The above sequence of keys do not contain duplicates,
	so every insertion will result in an addition of a
	node in the tree. The tree is unbalanced to the right 
	due to the nature of the sequence, which is inserted
	in alphabetical order. A rough representaiton of the 
	tree is shown, followed by functions to create
	the data structure (uncomment to run the code).

		Resulting tree

	a
	 \			   
	  b
	   \
		c
		 \
		  d
		   \
			e
		   	 \
		      f
		       \
		 		g
				 \	   
	  		 	  h
				   \ 
					i

CODE
val myTree2 = LEAF;
insert("a", myTree2);
insert("b", it);
insert("c", it);
insert("d", it);
insert("e", it);
insert("f", it);
insert("g", it);
insert("h", it);
insert("i", it);

RESULT
val myTree2 = LEAF : tree
val it = TREE (LEAF,"a",LEAF) : tree
val it = TREE (LEAF,"a",TREE (LEAF,"b",LEAF)) : tree
val it = TREE (LEAF,"a",TREE (LEAF,"b",TREE #)) : tree
val it = TREE (LEAF,"a",TREE (LEAF,"b",TREE #)) : tree
val it = TREE (LEAF,"a",TREE (LEAF,"b",TREE #)) : tree
val it = TREE (LEAF,"a",TREE (LEAF,"b",TREE #)) : tree
val it = TREE (LEAF,"a",TREE (LEAF,"b",TREE #)) : tree
val it = TREE (LEAF,"a",TREE (LEAF,"b",TREE #)) : tree
val it = TREE (LEAF,"a",TREE (LEAF,"b",TREE #)) : tree

*)

(* d *)
(* Red black trees. Red black trees are a type of self-balancing 
binary search trees that rebalances when the tree is modified, 
but not when lookup operations are performed. *)



