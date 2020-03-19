module GraphvizGenerator

let addParA s = function
    | Num _ | Var _ | UnaryMinus _ -> s
    | _                            -> sprintf "(%s)" s

let rec toStringA addPar aexp =
    if addPar then
        addParA (toStringA false aexp) aexp
    else
        match aexp with
            | Num n            -> sprintf "%i" n
            | Var v            -> v
            | Array (v, ind)   -> sprintf "%s[%s]"  v (toStringA true ind)
            | Plus (ex1, ex2)  -> sprintf "%s + %s" (toStringA true ex1) (toStringA true ex2)
            | Minus (ex1, ex2) -> sprintf "%s - %s" (toStringA true ex1) (toStringA true ex2)
            | Mul (ex1, ex2)   -> sprintf "%s * %s" (toStringA true ex1) (toStringA true ex2)
            | Div (ex1, ex2)   -> sprintf "%s / %s" (toStringA true ex1) (toStringA true ex2)
            | UnaryMinus n     -> sprintf "-%s"     (toStringA true n)
            | Pow (ex1, ex2)   -> sprintf "%s ^ %s" (toStringA true ex1) (toStringA true ex2)

let addParB s = function
    | TExp | FExp -> s
    | _           -> sprintf "(%s)" s

let rec toStringB addPar bexp = 
    if addPar then
        addParB (toStringB false bexp) bexp
    else
        match bexp with
            | TExp                    -> "true"
            | FExp                    -> "false"
            | AndExp (ex1, ex2)       -> sprintf "%s & %s"  (toStringB true ex1) (toStringB true ex2)
            | OrExp (ex1, ex2)        -> sprintf "%s | %s"  (toStringB true ex1) (toStringB true ex2)
            | ShortAndExp (ex1, ex2)  -> sprintf "%s && %s" (toStringB true ex1) (toStringB true ex2)
            | ShortOrExp (ex1, ex2)   -> sprintf "%s || %s" (toStringB true ex1) (toStringB true ex2)
            | NotExp ex               -> sprintf "!%s"      (toStringB true ex)
            | EqExp (ex1, ex2)        -> sprintf "%s = %s"  (toStringA true ex1) (toStringA true ex2)
            | NotEqExp (ex1, ex2)     -> sprintf "%s != %s" (toStringA true ex1) (toStringA true ex2)
            | GreaterExp (ex1, ex2)   -> sprintf "%s > %s"  (toStringA true ex1) (toStringA true ex2)
            | GreaterEqExp (ex1, ex2) -> sprintf "%s >= %s" (toStringA true ex1) (toStringA true ex2)
            | LessExp (ex1, ex2)      -> sprintf "%s < %s"  (toStringA true ex1) (toStringA true ex2)
            | LessEqExp (ex1, ex2)    -> sprintf "%s <= %s" (toStringA true ex1) (toStringA true ex2)

let rec toStringC = function
    | Assignment (var, aexp)           -> sprintf "%s := %s"     var (toStringA false aexp)
    | ArrayAssignment (var, ind, aexp) -> sprintf "%s[%s] := %s" var (toStringA false ind) (toStringA false aexp)
    | SkipExp                          -> "skip"
    | _                                -> failwith "wrong input"

let toString = function
    | A aexp -> toStringA false aexp
    | B bexp -> toStringB false bexp
    | C cexp -> toStringC cexp
    | _      -> failwith "wrong input"

let toStringN = function
    | StartNode -> "q▷"
    | EndNode   -> "q◀"
    | Node n    ->  sprintf "q%d" n

let rec generateGraphviz_aux s = function
    | []                -> sprintf "%s\n}" s
    | (q1, exp, q2)::xs -> let label = toString exp
                           let acc = sprintf "%s\r\n%s -> %s [label=\"%s\"];" s (toStringN q1) (toStringN q2) label
                           generateGraphviz_aux acc xs

let generateGraphviz edges =
    let header = "digraph program_graph {rankdir=LR;\nnode [shape = circle]; q▷;\nnode [shape = doublecircle]; q◀;\nnode [shape = circle]"
    generateGraphviz_aux header edges
