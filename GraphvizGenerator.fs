module GraphvizGenerator

let rec toStringA = function
    | Num n -> sprintf "%i" n
    | Var v -> v
    | Array (v, ind) -> sprintf "%s[%s]" v (toStringA ind)
    | Plus (ex1, ex2) -> sprintf "%s + %s" (toStringA ex1) (toStringA ex2)

let rec toStringB = function
    | TExp -> "true"
    | FExp -> "false"

let rec toStringC = function
    | Assignment(var, aexp) -> sprintf "%s := %s" var (toStringA aexp)
    | SkipExp -> "skip"


