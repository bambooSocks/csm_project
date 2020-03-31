module Interpreter
open System.Text.RegularExpressions
open GCLTypesAST

// Apply Arithmetic Operator To Arithmetic expressions
let rec ApplyArithOpToArith a1 a2 op mem : int option = 
    let z1 = ExecuteA mem a1
    let z2 = ExecuteA mem a2
    if z1.IsSome && z2.IsSome then
        Some (op z1.Value z2.Value)
    else 
        None

// Execute Arithmetic expression
and ExecuteA mem aexp : int option =
    match aexp with
    | Var v          -> Map.tryFind v mem
    | Num n          -> Some n
    | Add (a1, a2)   -> ApplyArithOpToArith a1 a2 ( + ) mem
    | Sub (a1, a2)   -> ApplyArithOpToArith a1 a2 ( - ) mem
    | Mul (a1, a2)   -> ApplyArithOpToArith a1 a2 ( * ) mem
    | Div (a1, a2)   -> ApplyArithOpToArith a1 a2 ( / ) mem     
    | Pow (a1, a2)   -> let z1 = ExecuteA mem a1
                        let z2 = ExecuteA mem a2
                        if z1.IsSome && z2.IsSome && (z2.Value >=0) then 
                            Some (int(float(z1.Value) ** float(z2.Value)))
                        else 
                            None 
    | Neg a1         -> let z1 = ExecuteA mem a1
                        if z1.IsSome then
                            Some (-(z1.Value))
                        else 
                            None
    | Arr (var, ind) -> let indVal = ExecuteA mem ind
                        if indVal.IsSome then
                            Map.tryFind (sprintf "%s[%u]" var indVal.Value) mem
                        else
                            None

// Apply Boolean Operator To Arithmetic expressions
let rec ApplyBoolOpToArith a1 a2 op mem : bool option =
    let z1 = ExecuteA mem a1
    let z2 = ExecuteA mem a2
    if z1.IsSome && z2.IsSome then
        Some (op z1.Value z2.Value)
    else
        None

// Apply Boolean Operator To Boolean expressions
and ApplyBoolOpToBool a1 a2 op mem : bool option = 
    let z1 = ExecuteB mem a1
    let z2 = ExecuteB mem a2
    if (z1.IsSome && z2.IsSome) then
        Some ( op z1.Value z2.Value)
    else
        None

// Apply Short-Circuit Boolean Operator To Boolean expressions
and ApplySCBoolOpToBool a1 a2 op mem :bool option = 
    let z1 = ExecuteB mem a1
    let z2 = ExecuteB mem a2
    if z1.IsNone then None
    else
        if z1.IsSome && z2.IsSome then
            Some (op z1.Value z2.Value)
        else
            Some false 

// Execute Boolean expression
and ExecuteB mem bexp : bool option =
    match bexp with
    | TExp           -> Some true
    | FExp           -> Some false
    | Eq (a1, a2)    -> ApplyBoolOpToArith a1 a2 (=) mem
    | NEq (a1,a2)    -> ApplyBoolOpToArith a1 a2 (<>) mem
    | Gr (a1,a2)     -> ApplyBoolOpToArith a1 a2 (>) mem
    | GrEq (a1,a2)   -> ApplyBoolOpToArith a1 a2 (>=) mem
    | Ls (a1,a2)     -> ApplyBoolOpToArith a1 a2 (<) mem
    | LsEq (a1,a2)   -> ApplyBoolOpToArith a1 a2 (<=) mem
    | Or (b1,b2)     -> ApplyBoolOpToBool b1 b2 (||) mem
    | And (b1, b2)   -> ApplyBoolOpToBool b1 b2 (&&) mem
    | SCOr (b1,b2)   -> ApplySCBoolOpToBool b1 b2 (||) mem
    | SCAnd (b1, b2) -> ApplySCBoolOpToBool b1 b2 (&&) mem
    | Not b1         -> let z = ExecuteB mem b1
                        if z.IsSome then
                            Some (not z.Value)
                        else 
                            None

// Execute general expression
let Execute mem exp : Memory option=
    match exp with
    | B bexp -> match (ExecuteB mem bexp) with
                | Some true -> Some mem
                | _         -> None
    | C cexp -> match cexp with
                | Skip                      -> Some mem
                | Asgmt (var, aexp)         -> match (ExecuteA mem aexp) with
                                                   | Some n -> Some (Map.add var n mem)
                                                   | _      -> None
                | ArrAsgmt (var, ind, aexp) -> match (ExecuteA mem ind), (ExecuteA mem aexp) with
                                                   | Some z1 , Some z2 -> let varName = sprintf "%s[%i]" var z1
                                                                          Some (Map.add varName z2 mem)
                                                   | _                 -> None
                | _                         -> failwith "wrong input"
    | _      -> failwith "wrong input"

// Creates a set of all used variable names in the abstract syntax tree
let rec CollectVariables exp output =
    match exp with
    | A aexp  -> match aexp with
                     | Var v                -> Set.add v output
                     | Arr (v, e)           -> CollectVariables (A e) (Set.add v output)
                     | Num _                -> output
                     | Neg e                -> CollectVariables (A e) output
                     | Add (e1, e2)
                     | Sub (e1, e2)
                     | Mul (e1, e2)
                     | Div (e1, e2)
                     | Pow (e1, e2)         -> CollectVariables (A e1) output
                                               |> CollectVariables (A e2)
    | B bexp  -> match bexp with
                     | TExp | FExp          -> output
                     | Not e                -> CollectVariables (B e) output
                     | And (e1, e2)
                     | Or (e1, e2)
                     | SCAnd (e1, e2)
                     | SCOr (e1, e2)        -> CollectVariables (B e1) output
                                               |> CollectVariables (B e2)
                     | Eq (e1, e2)
                     | NEq (e1, e2)
                     | Gr (e1, e2)
                     | GrEq (e1, e2)
                     | Ls (e1, e2)
                     | LsEq (e1, e2)        -> CollectVariables (A e1) output
                                               |> CollectVariables (A e2)
    | C cexp  -> match cexp with
                     | Asgmt (v, e)         -> CollectVariables (A e) (Set.add v output)
                     | ArrAsgmt (v, e1, e2) -> CollectVariables (A e1) (Set.add v output)
                                               |> CollectVariables (A e2)
                     | Skip                 -> output
                     | CSeq (e1, e2)        -> CollectVariables (C e1) output
                                               |> CollectVariables (C e2)
                     | If gc | Do gc        -> CollectVariables (G gc) output
    | G gcexp -> match gcexp with
                     | GC (g, c)            -> CollectVariables (B g) output
                                               |> CollectVariables (C c)
                     | GCSeq (g1, g2)       -> CollectVariables (G g1) output
                                               |> CollectVariables (G g2)

// Regex pattern match
// Modified from
// source: https://stackoverflow.com/questions/5684014/f-mapping-regular-expression-matches-with-active-patterns
let (|RegexMatch|_|) pattern input =
    if input = null then None
    else
        let m = Regex.Match(input, pattern)
        if m.Success then Some m
        else None


// Ask for integer or array value of the given variable
let rec InputInitVar v : (string * int) list =
    printf "Please enter an initial value for %s: " v
    match System.Console.ReadLine() with
    | RegexMatch "^(\[ *([0-9] *)+\])$" m -> let s = m.Value
                                                     |> fun str -> str.Replace ('[',' ')
                                                     |> fun str -> str.Replace (']',' ')
                                                     |> fun str -> str.Trim ()
                                                     |> fun str -> str.Split ' '
                                                     |> Array.toList
                                                     |> List.map (fun str -> int(str))
                                             let a = [0 .. (s.Length - 1)]
                                                     |> List.map (fun i -> sprintf "%s[%i]" v i)
                                             List.zip a s
    | RegexMatch "^[0-9]+$" m             -> [(v, int(m.Value))]
    | _                                   -> printfn "Wrong input! Enter either a number e.g. 1 or an array [ 1 2 3 ]"
                                             InputInitVar v

// Generate initial memory from all found variables
let GetInitVars exp =
    Set.toList (CollectVariables exp Set.empty)
    |> List.collect InputInitVar
    |> Map.ofList


// Run Program Graph defined by edges and given a current node q and memory mem
let rec RunPG q (edges: Set<Edge>) (mem: Memory) =
        let edgesList = Set.toList (Set.filter (fun (q1,exp,_) -> (q1 = q) && ((Execute mem exp).IsSome) ) edges)
        match edgesList with
        | []               -> match q with
                                  | EndNode -> Terminated mem
                                  | _       -> Stuck (q, mem)
        | (_, exp, q2)::_  -> (Execute mem exp).Value
                              |> RunPG q2 edges

// Prints the contents of memory
let PrintMemory m =
    Map.map (fun k v -> printfn "%s: %i" k v) m |> ignore

// Prints the given state
let PrintState = function
    | Terminated m -> printfn "Status: terminated"
                      printfn "Node: q◀"
                      PrintMemory m
    | Stuck (n, m) -> printfn "Status: stuck"
                      printfn "Node: %s" (ToStringNode n)
                      PrintMemory m
