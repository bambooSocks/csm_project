module PGGenerator
open GCLTypesAST
let mutable nodes = Set.empty // A set of used node numbers

// Creates a new node that would follow the given node
let rec NewNode node = 
    match node with
    | EndNode                              -> failwith("cannot create a node")
    | Node n when Set.contains (n+1) nodes -> NewNode (Node(n+1))
    | Node n                               -> nodes <- Set.add (n+1) nodes
                                              Node(n+1)

// Collects all guards from a Guarded Command expression to a Set of Boolean expressions 
let rec CollectGuards = function
    | GC (guard, _)    -> Set.ofList [guard]
    | GCSeq (gc1, gc2) -> Set.union (CollectGuards gc1) (CollectGuards gc2)

// Generates a condition (Boolean expression) for breaking a while loop from Guarded Command Expression
let rec GenerateDoneCondition = function
    | GC (guard, _)    -> Not guard
    | GCSeq (gc1, gc2) -> SCAnd ((GenerateDoneCondition gc1), (GenerateDoneCondition gc2))

// Adds edges for Command expressions from Node n1 to Node n2 into a set output
// isDet decides whether the output should be deterministic or not
let rec EdgesC n1 n2 output isDet = function
    | Skip                     -> Set.add (n1, C(Skip), n2) output
    | Asgmt (var, exp)         -> Set.add (n1, C(Asgmt(var, exp)), n2) output
    | ArrAsgmt (var, ind, exp) -> Set.add (n1, C(ArrAsgmt(var, ind, exp)), n2) output
    
    | CSeq (cmd1, cmd2)        -> let q = NewNode n1
                                  let tempOutput = EdgesC n1 q output isDet cmd1
                                  EdgesC q n2 tempOutput isDet cmd2

    | If gc                    -> let allBranches = CollectGuards gc
                                  EdgesG n1 n2 output isDet allBranches gc

    | Do gc                    -> let doneCond = GenerateDoneCondition gc
                                  let allBranches = CollectGuards gc
                                  let tempOutput = EdgesG n1 n1 output isDet allBranches gc
                                  Set.add (n1, B(doneCond) ,n2) tempOutput

// Adds edges for Guarded Command expressions from Node n1 to Node n2 into a set output
// isDet decides whether the output should be deterministic or not
// branches is a set of all guards of the master guarded command, which is used to generate deterministic program graphes
and EdgesG n1 n2 output isDet branches = function
    // Deterministic Guarded Command
    | GC (guard, cmd) when isDet -> let q = NewNode n1
                                    // make a set of all guards without the current guard
                                    let otherBranches = Set.difference branches (Set.empty.Add(guard))
                                    // create an extra deterministic condition for the guard
                                    let detCond = List.foldBack (fun b acc -> SCOr (b, acc)) (Set.toList otherBranches) FExp
                                    // add a new edge with the full deterministic condition to the new temporary set of edges
                                    let tempOutput = Set.add (n1, B(SCAnd(guard, (Not detCond))), q) output
                                    EdgesC q n2 tempOutput isDet cmd

    // Non-deterministic Guarded Command
    | GC (guard, cmd)            -> let q = NewNode n1
                                    let tempOutput = Set.add (n1, B(guard), q) output
                                    EdgesC q n2 tempOutput isDet cmd

    // Sequence of Guarded Commands
    | GCSeq (gc1, gc2)           -> let tempOutput = EdgesG n1 n2 output isDet branches gc1 
                                    EdgesG n1 n2 tempOutput isDet branches gc2
