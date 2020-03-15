module PGGenerator

let mutable nodeNumbers = Set.empty //set of node numbers

type NodeType = | StartNode
                | EndNode
                | Node of int

let rec newNode node = 
    match node with
    | StartNode -> Node(1)
    | EndNode -> failwith("cannot append a node")
    | Node n -> if Set.contains (n+1) nodeNumbers then
                    newNode (Node(n+1))
                else
                    nodeNumbers <- Set.add (n+1) nodeNumbers 
                    Node(n+1)

let rec edgesC node1 node2 output = function
    | Assignment(var, exp) -> Set.add (node1, C(Assignment(var, exp)), node2) output
    | CExpSeq(cmd1, cmd2) -> let q = newNode node1
                             let intermediate = edgesC node1 q output cmd1
                             edgesC q node2 intermediate cmd2
    | SkipExp -> Set.add(node1,C(SkipExp),node2) output
    | ArrayAssignment(var,ind,exp) -> Set.add(node1,C(ArrayAssignment(var,ind,exp)),node2) output
    | IfExp (gexp) -> edgesG node1 node2 output gexp
    |  DoExp (gexp)-> let (bexp,cexp) = gexp 
                      let intermediate = edgesG node1 node1 output gexp 
                      Set.add (node1,B(NotExp bexp),node2) intermediate
                   
and edgesG node1 node2 output = function
    | GC (guard, cmd) -> let q = newNode node1
                         let s = Set.add (node1, B(guard), q) output
                         edgesC q node2 s cmd
    | GC (gcmd1,gcmd2) -> let e1 = edgesG node1 node2 output gcmd1 
                          edgesG node1 node2 e1 gcmd2
                          