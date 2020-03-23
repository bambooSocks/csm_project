module PGGenerator

let mutable nodes = Set.empty //set of node numbers

type NodeType = | EndNode
                | Node of int

let rec newNode node = 
    match node with
    | EndNode                              -> failwith("cannot append a node")
    | Node n when Set.contains (n+1) nodes -> newNode (Node(n+1))
    | Node n                               -> nodes <- Set.add (n+1) nodes
                                              Node(n+1)
                

let rec colGuards = function
    | GC (guard, _)    -> Set.ofList [guard]
    | GCSeq (gc1, gc2) -> Set.union (colGuards gc1) (colGuards gc2)

let rec doneGen = function
    | GC (guard, _)    -> NotExp guard
    | GCSeq (gc1, gc2) -> ShortAndExp ((doneGen gc1), (doneGen gc2))

let rec detGuard = function
    | []    -> FExp
    | b::bs -> ShortOrExp (b, (detGuard bs))

let rec edgesC n1 n2 output isDet = function
    | Assignment(var, exp)          -> Set.add (n1, C(Assignment(var, exp)), n2) output
    | CExpSeq(cmd1, cmd2)           -> let q = newNode n1
                                       let aux = edgesC n1 q output isDet cmd1
                                       edgesC q n2 aux isDet cmd2

    | SkipExp                       -> Set.add(n1,C(SkipExp),n2) output
    | ArrayAssignment(var,ind,exp)  -> Set.add(n1,C(ArrayAssignment(var,ind,exp)),n2) output
    | IfExp (guard)                 -> let allBranches = colGuards guard
                                       edgesG n1 n2 output isDet allBranches guard

    | DoExp (guard)                 -> let doneExp = doneGen guard
                                       let allBranches = colGuards guard
                                       let aux = edgesG n1 n1 output isDet allBranches guard 
                                       Set.add (n1, B(doneExp) ,n2) aux
                   
and edgesG n1 n2 output isDet d = function
    | GC (guard, cmd) when isDet -> let q = newNode n1  
                                    let setWoGuards = Set.difference d (Set.ofList [guard])
                                    let dexp = detGuard (Set.toList setWoGuards) 
                                    let s = Set.add (n1, B(ShortAndExp(guard, (NotExp dexp))), q) output
                                    edgesC q n2 s isDet cmd

    | GC (guard, cmd)            -> let s = Set.add (n1, B(guard), q) output
                                    edgesC q n2 s isDet cmd                      
                              
    | GCSeq (gc1, gc2)           -> let e1 = edgesG n1 n2 output isDet d gc1 
                                    edgesG n1 n2 e1 isDet d gc2
                          
