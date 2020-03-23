module Interpreter

// returns option int
let execA mem = function
    | Var v -> if (Map.containsKey v mem) then
                   Some (Map.find v mem)
               else
                   None
    | Num n -> Some n
    // TODO: implement this

// returns option boolean
let rec execB mem = function
    | TExp            -> Some true
    | FExp            -> Some false
    | AndExp (b1, b2) -> let ob1 = execB mem b1
                         let ob2 = execB mem b2
                         if (ob1.IsNone || ob2.IsNone) then
                            None
                         else
                            Some (ob1.Value && ob2.Value)
    | EqExp (a1, a2)  -> let oa1 = execA mem a1
                         let oa2 = execA mem a2
                         if (oa1.IsNone || oa2.IsNone) then
                            None
                         else
                            Some (oa1.Value = oa2.Value)
    // TODO: implement this
    | _ -> None

// returns option memory (Map string and int)
let exec mem = function
    | B bexp -> match (execB mem bexp) with
                | Some true -> Some mem
                | _         -> None
    | C cexp -> match cexp with
                | SkipExp                          -> Some mem
                | Assignment (var, aexp)           -> match (execA mem aexp) with
                                                      | Some n when mem.ContainsKey var -> Some (Map.add var n mem)
                                                      | _                               -> None
                | ArrayAssignment (var, ind, aexp) -> None // TODO: implement this
                | _                                -> failwith "wrong input"
    | _      -> failwith "wrong input"
    

let chooseBranch q mem edges = 
    Set.filter (fun (q1,exp,_) -> (q1 = q) && ((exec mem exp).IsSome) ) edges 

// let testExec =
    // printfn "%A" (execB (Map.ofList [("x",0)]) (EqExp (Var "y", Num 5) ) )
    // printfn "%A" (exec (Map.ofList [("x",0)]) (B (ShortAndExp (GreaterEqExp (Var "x", Num 0),NotExp (ShortOrExp (EqExp (Var "x", Num 0), ShortOrExp (LessEqExp (Var "x", Num 0), FExp))))))


