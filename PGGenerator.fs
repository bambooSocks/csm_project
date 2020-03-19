module PGGenerator

let mutable det = true
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

let rec collectGuards = function
    | GC (guard, _)    -> Set.ofList [guard]
    | GCSeq (gc1, gc2) -> Set.union (collectGuards gc1) (collectGuards gc2)

let rec doneGen = function
    | GC (guard, _)    -> NotExp guard
    | GCSeq (gc1, gc2) -> ShortAndExp ((doneGen gc1), (doneGen gc2))

let rec detGuard = function
    | []    -> FExp
    | b::bs -> ShortOrExp (b, (detGuard bs))

let rec edgesC node1 node2 output = function
    | Assignment(var, exp) -> Set.add (node1, C(Assignment(var, exp)), node2) output
    | CExpSeq(cmd1, cmd2) -> let q = newNode node1
                             let intermediate = edgesC node1 q output cmd1
                             edgesC q node2 intermediate cmd2
    | SkipExp -> Set.add(node1,C(SkipExp),node2) output
    | ArrayAssignment(var,ind,exp) -> Set.add(node1,C(ArrayAssignment(var,ind,exp)),node2) output
    | IfExp (gexp) -> let all_branches = collectGuards gexp
                      edgesG node1 node2 output Set.empty gexp
    | DoExp (gexp) -> let doneExp = doneGen gexp
                      let all_branches = collectGuards gexp
                      let intermediate = edgesG node1 node1 output all_branches gexp 
                      Set.add (node1, B(doneExp) ,node2) intermediate
                   
and edgesG node1 node2 output d = function
    | GC (guard, cmd)  -> let q = newNode node1
                          if det then
                            let set_wo_guard = Set.difference d (Set.ofList [guard])
                            let dexp = detGuard (Set.toList set_wo_guard)
                            let s = Set.add (node1, B(ShortAndExp(guard, (NotExp dexp))), q) output
                            edgesC q node2 s cmd
                          else
                            let s = Set.add (node1, B(guard), q) output
                            edgesC q node2 s cmd
    | GCSeq (gc1, gc2) -> let e1 = edgesG node1 node2 output d gc1 
                          edgesG node1 node2 e1 d gc2
                          