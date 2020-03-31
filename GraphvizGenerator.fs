module GraphvizGenerator
open GCLTypesAST

// Adds parentheses to specific Arithmetic expressions
let AddParenthesisToArith s = function
    | Num _ | Var _ | Neg _ -> s
    | _                     -> sprintf "(%s)" s

// Converts an Arithmetic expression to string
let rec ToStringArith addPar aexp =
    if addPar then
        AddParenthesisToArith (ToStringArith false aexp) aexp
    else
        match aexp with
            | Num n          -> sprintf "%i" n
            | Var v          -> v
            | Arr (v, ind)   -> sprintf "%s[%s]"  v (ToStringArith false ind)
            | Add (ex1, ex2) -> sprintf "%s + %s" (ToStringArith true ex1) (ToStringArith true ex2)
            | Sub (ex1, ex2) -> sprintf "%s - %s" (ToStringArith true ex1) (ToStringArith true ex2)
            | Mul (ex1, ex2) -> sprintf "%s * %s" (ToStringArith true ex1) (ToStringArith true ex2)
            | Div (ex1, ex2) -> sprintf "%s / %s" (ToStringArith true ex1) (ToStringArith true ex2)
            | Neg n          -> sprintf "-%s"     (ToStringArith true n)
            | Pow (ex1, ex2) -> sprintf "%s ^ %s" (ToStringArith true ex1) (ToStringArith true ex2)

// Adds parentheses to specific Boolean expressions
let AddParenthesisToBool s = function
    | TExp | FExp | Not _ -> s
    | _                   -> sprintf "(%s)" s

// Converts a Boolean expression to string
let rec ToStringBool addPar bexp = 
    if addPar then
        AddParenthesisToBool (ToStringBool false bexp) bexp
    else
        match bexp with
            | TExp             -> "true"
            | FExp             -> "false"
            | And (ex1, ex2)   -> sprintf "%s & %s"  (ToStringBool true ex1) (ToStringBool true ex2)
            | Or (ex1, ex2)    -> sprintf "%s | %s"  (ToStringBool true ex1) (ToStringBool true ex2)
            | SCAnd (ex1, ex2) -> sprintf "%s && %s" (ToStringBool true ex1) (ToStringBool true ex2)
            | SCOr (ex1, ex2)  -> sprintf "%s || %s" (ToStringBool true ex1) (ToStringBool true ex2)
            | Not ex           -> sprintf "!%s"      (ToStringBool true ex)
            | Eq (ex1, ex2)    -> sprintf "%s = %s"  (ToStringArith true ex1) (ToStringArith true ex2)
            | NEq (ex1, ex2)   -> sprintf "%s != %s" (ToStringArith true ex1) (ToStringArith true ex2)
            | Gr (ex1, ex2)    -> sprintf "%s > %s"  (ToStringArith true ex1) (ToStringArith true ex2)
            | GrEq (ex1, ex2)  -> sprintf "%s >= %s" (ToStringArith true ex1) (ToStringArith true ex2)
            | Ls (ex1, ex2)    -> sprintf "%s < %s"  (ToStringArith true ex1) (ToStringArith true ex2)
            | LsEq (ex1, ex2)  -> sprintf "%s <= %s" (ToStringArith true ex1) (ToStringArith true ex2)

// Converts a Command expression to string
let rec ToStringCmd = function
    | Asgmt (var, aexp)         -> sprintf "%s := %s"     var (ToStringArith false aexp)
    | ArrAsgmt (var, ind, aexp) -> sprintf "%s[%s] := %s" var (ToStringArith false ind) (ToStringArith false aexp)
    | Skip                      -> "skip"
    | _                         -> failwith "wrong input"

// Converts a general expression to string
let ToStringExp = function
    | A aexp -> ToStringArith false aexp
    | B bexp -> ToStringBool false bexp
    | C cexp -> ToStringCmd cexp
    | _      -> failwith "wrong input"

// Generates Graphviz graph from set of edges
let GenerateGraphviz edges =
    List.fold (fun acc (q1, exp, q2) -> sprintf "%s\r\n%s -> %s [label=\"%s\"];" acc (ToStringNode q1) (ToStringNode q2) (ToStringExp exp))
              "digraph program_graph {rankdir=LR;\nnode [shape = circle]; q▷;\nnode [shape = doublecircle]; q◀;\nnode [shape = circle]"
              (Set.toList edges) |> sprintf "%s\n}" 
    
