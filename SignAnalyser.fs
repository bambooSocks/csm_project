module SignAnalyser
open GCLTypesAST
open System.Text.RegularExpressions
open Signs

let sign n =
    if n > 0 then Plus
    else if n < 0 then Minus
    else Zero

let crossProduct s1 s2 =
    seq {
        for i in s1 do
            for j in s2 do
                set[(i, j)]
    }
    |> Set.unionMany

let rec SignApplyArithOpToArith a1 a2 f mem =
    let s1 = SignAnalyseA mem a1
    let s2 = SignAnalyseA mem a2
    Set.foldBack (fun y acc -> Set.union acc (f y)) (crossProduct s1 s2) Set.empty

and SignAnalyseA (m1: AbstractVariableMemory, m2: AbstractArrayMemory) aexp: Set<Sign> =
    match aexp with
        | Num n        -> set [sign n]
        | Var v        -> match Map.tryFind v m1 with
                             | Some s -> set [s]
                             | None   -> Set.empty // maybe error?
        | Add (a1, a2) -> SignApplyArithOpToArith a1 a2 AddSigns (m1, m2)
        | Sub (a1, a2) -> SignApplyArithOpToArith a1 a2 SubSigns (m1, m2)
        | Mul (a1, a2) -> SignApplyArithOpToArith a1 a2 MulSigns (m1, m2)
        | Div (a1, a2) -> SignApplyArithOpToArith a1 a2 DivSigns (m1, m2)
        | Pow (a1, a2) -> SignApplyArithOpToArith a1 a2 PowSigns (m1, m2)
        | Neg a        -> let s = SignAnalyseA (m1, m2) a
                          Set.foldBack (fun y acc -> Set.union acc (NegSigns y)) s Set.empty
        | Arr (v, a)   -> let indSigns = SignAnalyseA (m1, m2) a
                          if not ((Set.contains Plus indSigns) || (Set.contains Zero indSigns)) then
                              Set.empty
                          else
                              match Map.tryFind v m2 with 
                                 | Some s -> s
                                 | None   -> Set.empty
 
let rec SignApplyBoolOpToArith a1 a2 f mem =
    let s1 = SignAnalyseA mem a1
    let s2 = SignAnalyseA mem a2
    Set.foldBack (fun y acc -> Set.union acc (f y)) (crossProduct s1 s2) Set.empty
    
and SignApplyBoolOpToBool b1 b2 f mem =
    let s1 = SignAnalyseB mem b1
    let s2 = SignAnalyseB mem b2
    Set.foldBack (fun y acc -> Set.union acc (f y)) (crossProduct s1 s2) Set.empty
    
and SignAnalyseB (m1: AbstractVariableMemory, m2: AbstractArrayMemory) bexp: Set<BoolSign> =
    match bexp with
        | TExp  -> set [TrueSign]
        | FExp -> set [FalseSign]
        | Eq (a1, a2)    -> SignApplyBoolOpToArith a1 a2 EqSigns (m1, m2)
        | NEq (a1, a2)   -> SignApplyBoolOpToArith a1 a2 NeqSigns (m1, m2)
        | Gr (a1, a2)    -> SignApplyBoolOpToArith a1 a2 GrSigns (m1, m2)
        | GrEq (a1, a2)  -> SignApplyBoolOpToArith a1 a2 GrEqSigns (m1, m2)
        | Ls (a1, a2)    -> SignApplyBoolOpToArith a1 a2 LsSigns (m1, m2)
        | LsEq (a1, a2)  -> SignApplyBoolOpToArith a1 a2 LsEqSigns (m1, m2)
        | Or (b1, b2)    -> SignApplyBoolOpToBool b1 b2 OrSigns (m1, m2)
        | And (b1, b2)   -> SignApplyBoolOpToBool b1 b2 AndSigns (m1, m2)
        | SCOr (b1, b2)  -> SignApplyBoolOpToBool b1 b2 SCOrSigns (m1, m2)
        | SCAnd (b1, b2) -> SignApplyBoolOpToBool b1 b2 SCAndSigns (m1, m2)
        | Not b          -> let s = SignAnalyseB (m1, m2) b
                            Set.foldBack (fun y acc -> Set.union acc (NotSigns y)) s Set.empty
                               
let SignAnalyse (mem:AbstractMemory list) exp =
    match exp with
    | B bexp -> List.collect (fun m -> if Set.contains TrueSign (SignAnalyseB m bexp) then [m] else []) mem
    | C cexp -> match cexp with
                | Skip                      -> mem
                | Asgmt (var, aexp)         -> let analyse (m1, m2) =
                                                   let signToMem s =
                                                       if Map.containsKey var m1 then
                                                           [((Map.add var s m1), m2)]
                                                       else
                                                           []
                                                   SignAnalyseA (m1, m2) aexp
                                                   |> Set.toList
                                                   |> List.collect signToMem
                                               List.collect analyse mem
                | ArrAsgmt (var, ind, aexp) -> let analyse (m1, m2) =
                                                   let indSigns = SignAnalyseA (m1, m2) ind
                                                   if not ((Set.contains Plus indSigns) || (Set.contains Zero indSigns)) then
                                                       []
                                                   else
                                                       let s = SignAnalyseA (m1, m2) aexp
                                                       if Map.containsKey var m2 then
                                                           let old = Map.find var m2
                                                           [(m1, (Map.add var (Set.union old s) m2))]
                                                       else
                                                           []
                                               List.collect analyse mem
                | _                         -> failwith "wrong input"
    | _      -> failwith "wrong input"
    
// Creates a set of all used variable names in the abstract syntax tree
let rec CollectVariablesArrays exp (var, arr) =
    match exp with
    | A aexp  -> match aexp with
                     | Var v                -> (Set.add v var, arr)
                     | Arr (v, e)           -> CollectVariablesArrays (A e) (var, (Set.add v arr))
                     | Num _                -> (var, arr)
                     | Neg e                -> CollectVariablesArrays (A e) (var, arr)
                     | Add (e1, e2)
                     | Sub (e1, e2)
                     | Mul (e1, e2)
                     | Div (e1, e2)
                     | Pow (e1, e2)         -> CollectVariablesArrays (A e1) (var, arr)
                                               |> CollectVariablesArrays (A e2)
    | B bexp  -> match bexp with
                     | TExp | FExp          -> (var, arr)
                     | Not e                -> CollectVariablesArrays (B e) (var, arr)
                     | And (e1, e2)
                     | Or (e1, e2)
                     | SCAnd (e1, e2)
                     | SCOr (e1, e2)        -> CollectVariablesArrays (B e1) (var, arr)
                                               |> CollectVariablesArrays (B e2)
                     | Eq (e1, e2)
                     | NEq (e1, e2)
                     | Gr (e1, e2)
                     | GrEq (e1, e2)
                     | Ls (e1, e2)
                     | LsEq (e1, e2)        -> CollectVariablesArrays (A e1) (var, arr)
                                               |> CollectVariablesArrays (A e2)
    | C cexp  -> match cexp with
                     | Asgmt (v, e)         -> CollectVariablesArrays (A e) ((Set.add v var), arr)
                     | ArrAsgmt (v, e1, e2) -> CollectVariablesArrays (A e1) (var, (Set.add v arr))
                                               |> CollectVariablesArrays (A e2)
                     | Skip                 -> (var, arr)
                     | CSeq (e1, e2)        -> CollectVariablesArrays (C e1) (var, arr)
                                               |> CollectVariablesArrays (C e2)
                     | If gc | Do gc        -> CollectVariablesArrays (G gc) (var, arr)
    | G gcexp -> match gcexp with
                     | GC (g, c)            -> CollectVariablesArrays (B g) (var, arr)
                                               |> CollectVariablesArrays (C c)
                     | GCSeq (g1, g2)       -> CollectVariablesArrays (G g1) (var, arr)
                                               |> CollectVariablesArrays (G g2)

let InputToSign = function
    | "+" -> Plus
    | "0" -> Zero
    | "-" -> Minus
    | _ -> failwith "wrong input to InputToSign"
    
let OutputFromSign = function
    | Plus -> "+"
    | Zero -> "0"
    | Minus -> "-"
    
let OutputFromSetSign s =
    Set.map OutputFromSign s
    |> Set.toList
    |> String.concat " "
    |> sprintf "{%s}" 

// Ask for integer or array value of the given variable
let rec InputInitSignVar v : (string * Sign) list =
    printf "Please enter an initial value for %s: " v
    let m = Regex.Match(System.Console.ReadLine(), "^ *[-0+] *$")
    if m.Success then
        let s = m.Value.Trim()
        [(v, (InputToSign s))]
    else printfn "Wrong input! Enter a sign e.g. +, 0 or -"
         InputInitSignVar v
         
let rec InputInitSignArr a : (string * Set<Sign>) list =
    printf "Please enter an initial values for %s: " a
    let m = Regex.Match(System.Console.ReadLine(), "^( *\[ *([-0+] *)+\] *)$")
    if m.Success then
        let s = m.Value
                |> fun str -> str.Replace ('[',' ')
                |> fun str -> str.Replace (']',' ')
                |> fun str -> str.Trim ()
                |> fun str -> str.Split ' '
                |> Array.toList
                |> List.map InputToSign
                |> Set.ofList
        [(a, s)]
    else printfn "Wrong input! Enter an array of signs e.g. [ + 0 - ]"
         InputInitSignArr a
        
// Generate initial memory from all found variables
let GetInitSignVarsArrs exp : AbstractMemory list =
    let (v, a) = CollectVariablesArrays exp (Set.empty, Set.empty)
    let m1 = v
             |> Set.toList
             |> List.collect InputInitSignVar
             |> Map.ofList
    let m2 = a
             |> Set.toList
             |> List.collect InputInitSignArr
             |> Map.ofList
    [(m1, m2)]
    
// Run Program Graph defined by edges and given a current node q and memory mem
let rec RunSignAnalysisOnPG q (edges: Set<Edge>) (mem: AbstractMemory list) =
    match q with
        | Node 0 -> mem
        | N      -> let currentEdges = edges
                                       |> Set.filter (fun (_, _, q') -> q' = N)
                    let previousMem = currentEdges
                                      |> Set.map (fun (q', _, _) -> q')
                                      |> Set.toList
                                      |> List.collect (fun q' -> RunSignAnalysisOnPG q' edges mem)
                    let currentExps = currentEdges
                                      |> Set.map (fun (_, e, _) -> e)
                    List.collect (SignAnalyse previousMem) (Set.toList currentExps)

let PrintAbstractMemory (m1, m2) =
    Map.iter (fun _ v -> printf " %s " (OutputFromSign v)) m1
    Map.iter (fun _ v -> printf "%s" (OutputFromSetSign v)) m2
    printfn ""
    
let PrintAbstractMemories mem =
    let (m1, m2) = List.head mem
    Map.iter (fun k _ -> printf " %s " k) m1
    Map.iter (fun k _ -> printf "   %s   " k) m2
    printfn ""
    List.iter PrintAbstractMemory mem
