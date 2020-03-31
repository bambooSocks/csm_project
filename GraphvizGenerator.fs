module GraphvizGenerator
open GCLTypesAST

let addParA s = function
    | Num _ | Var _ | Neg _ -> s
    | _                            -> sprintf "(%s)" s

let rec toStringA addPar aexp =
    if addPar then
        addParA (toStringA false aexp) aexp
    else
        match aexp with
            | Num n            -> sprintf "%i" n
            | Var v            -> v
            | Arr (v, ind)   -> sprintf "%s[%s]"  v (toStringA true ind)
            | Add (ex1, ex2)  -> sprintf "%s + %s" (toStringA true ex1) (toStringA true ex2)
            | Sub (ex1, ex2) -> sprintf "%s - %s" (toStringA true ex1) (toStringA true ex2)
            | Mul (ex1, ex2)   -> sprintf "%s * %s" (toStringA true ex1) (toStringA true ex2)
            | Div (ex1, ex2)   -> sprintf "%s / %s" (toStringA true ex1) (toStringA true ex2)
            | Neg n     -> sprintf "-%s"     (toStringA true n)
            | Pow (ex1, ex2)   -> sprintf "%s ^ %s" (toStringA true ex1) (toStringA true ex2)

let addParB s = function
    | TExp | FExp | Not _ -> s
    | _                      -> sprintf "(%s)" s

let rec toStringB addPar bexp = 
    if addPar then
        addParB (toStringB false bexp) bexp
    else
        match bexp with
            | TExp                    -> "true"
            | FExp                    -> "false"
            | And (ex1, ex2)       -> sprintf "%s & %s"  (toStringB true ex1) (toStringB true ex2)
            | Or (ex1, ex2)        -> sprintf "%s | %s"  (toStringB true ex1) (toStringB true ex2)
            | SCAnd (ex1, ex2)  -> sprintf "%s && %s" (toStringB true ex1) (toStringB true ex2)
            | SCOr (ex1, ex2)   -> sprintf "%s || %s" (toStringB true ex1) (toStringB true ex2)
            | Not ex               -> sprintf "!%s"      (toStringB true ex)
            | Eq (ex1, ex2)        -> sprintf "%s = %s"  (toStringA true ex1) (toStringA true ex2)
            | NEq (ex1, ex2)     -> sprintf "%s != %s" (toStringA true ex1) (toStringA true ex2)
            | Gr (ex1, ex2)   -> sprintf "%s > %s"  (toStringA true ex1) (toStringA true ex2)
            | GrEq (ex1, ex2) -> sprintf "%s >= %s" (toStringA true ex1) (toStringA true ex2)
            | Ls (ex1, ex2)      -> sprintf "%s < %s"  (toStringA true ex1) (toStringA true ex2)
            | LsEq (ex1, ex2)    -> sprintf "%s <= %s" (toStringA true ex1) (toStringA true ex2)

let rec toStringC = function
    | Asgmt (var, aexp)           -> sprintf "%s := %s"     var (toStringA false aexp)
    | ArrAsgmt (var, ind, aexp) -> sprintf "%s[%s] := %s" var (toStringA false ind) (toStringA false aexp)
    | Skip                          -> "skip"
    | _                                -> failwith "wrong input"

let toString = function
    | A aexp -> toStringA false aexp
    | B bexp -> toStringB false bexp
    | C cexp -> toStringC cexp
    | _      -> failwith "wrong input"

let rec generateGraphviz_aux s = function
    | []                -> sprintf "%s\n}" s
    | (q1, exp, q2)::xs -> let acc = sprintf "%s\r\n%s -> %s [label=\"%s\"];" s (toStringN q1) (toStringN q2) (toString exp)
                           generateGraphviz_aux acc xs

let generateGraphviz edges =
    let header = "digraph program_graph {rankdir=LR;\nnode [shape = circle]; q▷;\nnode [shape = doublecircle]; q◀;\nnode [shape = circle]"
    generateGraphviz_aux header edges
