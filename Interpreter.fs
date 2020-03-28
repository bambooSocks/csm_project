module Interpreter

let rec applyOpA a1 a2 op mem = 
    let z1 = execA mem a1
    let z2 = execA mem a2
    if z1.IsSome && z2.IsSome then 
       Some (op z1.Value z2.Value)
    else 
       None 


// returns option int
and execA mem aexp : int option =
    match aexp with
    | Var v            -> if (Map.containsKey v mem) then
                             Some (Map.find v mem)
                          else
                             None
    | Num n            -> Some n
    | Plus (a1,a2)     -> applyOpA a1 a2 (+) mem
    | Minus (a1,a2)    -> applyOpA a1 a2 (-) mem
    | Mul (a1,a2)      -> applyOpA a1 a2 ( * ) mem
    | Div (a1,a2)      -> applyOpA a1 a2 (/) mem     
    | Pow (a1,a2)      -> let z1 = execA mem a1
                          let z2 = execA mem a2
                          if z1.IsSome && z2.IsSome && (z2.Value >=0) then 
                             Some (int(float(z1.Value) ** float(z2.Value)))
                          else 
                             None 
    | UnaryMinus a1    -> let z1 = execA mem a1
                          if z1.IsSome then
                             Some (-(z1.Value))
                          else 
                             None
    | Array (var, ind) -> let indVal = execA mem ind
                          if indVal.IsSome then
                              let varName = (sprintf "%s[%u]" var indVal.Value)
                              if mem.ContainsKey varName then
                                  Some (Map.find varName mem)  
                              else
                                  None
                          else
                              None

  
let rec applyOpB a1 a2 op mem : bool option = 
               let z1 = execB mem a1
               let z2 = execB mem a2
               if (z1.IsSome && z2.IsSome) then
                  Some ( op z1.Value z2.Value)
               else 
                  None  
and applyOpAB a1 a2 op mem : bool option =
    let z1 = execA mem a1
    let z2 = execA mem a2
    if z1.IsSome && z2.IsSome then 
       Some (op z1.Value z2.Value)
    else 
       None  

// returns option boolean
//no distinction between logical conjunction and short circuit and/or
and execB mem bexp : bool option =
    match bexp with
    | TExp                 -> Some true
    | FExp                 -> Some false
    | EqExp (a1, a2)       -> applyOpAB a1 a2 (=) mem
    | NotEqExp (a1,a2)     -> applyOpAB a1 a2 (<>) mem
    | GreaterExp (a1,a2)   -> applyOpAB a1 a2 (>) mem
    | GreaterEqExp (a1,a2) -> applyOpAB a1 a2 (>=) mem
    | LessExp (a1,a2)      -> applyOpAB a1 a2 (<) mem
    | LessEqExp (a1,a2)    -> applyOpAB a1 a2 (<=) mem                   
    | OrExp (b1,b2)        -> applyOpB b1 b2 (||) mem
    | ShortOrExp (b1,b2)   -> applyOpB b1 b2 (||) mem                                                       
    | AndExp (b1, b2)      -> applyOpB b1 b2 (&&) mem
    | ShortAndExp (b1, b2) -> applyOpB b1 b2 (&&) mem   
    | NotExp b1            -> None // FIXME: Some (not(execB mem b1))                  

// returns option memory (Map string and int)
let exec mem exp : Map<string, int> option=
    match exp with
    | B bexp -> match (execB mem bexp) with
                | Some true -> Some mem
                | _         -> None
    | C cexp -> match cexp with
                | SkipExp                          -> Some mem
                | Assignment (var, aexp)           -> match (execA mem aexp) with
                                                      | Some n when mem.ContainsKey var -> Some (Map.add var n mem)
                                                      | _                               -> None
                | ArrayAssignment (var, ind, aexp) -> let z1 = execA mem ind
                                                      let z2 = execA mem aexp
                                                      let varName = sprintf "%s[%i]" var z1.Value
                                                      if z1.IsSome && z2.IsSome && (mem.ContainsKey varName) then
                                                          Some (Map.add varName z2.Value mem)
                                                      else 
                                                          None 
                | _                                -> failwith "wrong input"
    | _      -> failwith "wrong input"
    

let chooseBranch q mem edges = 
    Set.filter (fun (q1,exp,_) -> (q1 = q) && ((exec mem exp).IsSome) ) edges 

let testExec =
    printfn "%A" (execA (Map.ofList [("x",0);("y",3)]) (Plus (Var "y", Num 5) ) )
    // printfn "%A" (exec (Map.ofList [("x",0)]) (B (ShortAndExp (GreaterEqExp (Var "x", Num 0),NotExp (ShortOrExp (EqExp (Var "x", Num 0), ShortOrExp (LessEqExp (Var "x", Num 0), FExp))))))


