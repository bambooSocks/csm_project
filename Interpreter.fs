module Interpreter

// returns option int
let rec execA mem = function
    | Var v            -> if (Map.containsKey v mem) then
                             Some (Map.find v mem)
                          else
                             None
    | Num n            -> Some n
    | Plus (a1,a2)     -> let z1 = execA mem a1
                          let z2 = execA mem a2
                          if z1.IsSome && z2.IsSome then 
                             Some (z1.Value + z2.Value)
                          else 
                             None 
    | Minus (a1,a2)    -> let z1 = execA mem a1
                          let z2 = execA mem a2
                          if z1.IsSome && z2.IsSome then 
                             Some (z1.Value - z2.Value)
                          else 
                             None 
    | Mul (a1,a2)      -> let z1 = execA mem a1
                          let z2 = execA mem a2
                          if z1.IsSome && z2.IsSome then 
                             Some (z1.Value * z2.Value)
                          else 
                             None  
    | Div (a1,a2)      -> let z1 = execA mem a1
                          let z2 = execA mem a2
                          if z1.IsSome && z2.IsSome then 
                             Some (z1.Value / z2.Value)
                          else 
                             None       
    | Pow (a1,a2)      -> let z1 = execA mem a1
                          let z2 = execA mem a2
                          if z1.IsSome && z2.IsSome && (z2.Value >=0) then 
                          let res = flzt(z1.Value) ** flzt(z2.Value)
                             Some (int(res))
                          else 
                             None 
    | UnaryMinus a1    -> let z1 = execA mem a1
                          if z1.IsSome
                             Some (-z1)
                          else 
                             None
    | Array (var, ind) -> if mem.containsKey var then 
                             Some((Map.find var mem).[ind]) 
                          else
                             None


  

// returns option boolean
//no distinction between logical conjunction and short circuit and/or
let rec execB mem = function
    | TExp                 -> Some true
    | FExp                 -> Some false
    | EqExp (a1, a2)       -> let z1 = execA mem a1
                              let z2 = execA mem a2
                              if (z1.IsNone || z2.IsNone) then
                                 None
                              else
                                 Some (z1.Value = z2.Value)
    | NotEqExp (a1,a2)     -> let z1 = execA mem a1
                              let z2 = execA mem a2
                              if (z1.IsNone || z2.IsNone) then
                                None
                              else
                                 Some (not(z1.Value = z2.Value))
    | GreaterExp (a1,a2)   -> let z1 = execA mem a1
                              let z2 = execA mem a2
                              if (z1.IsNone || z2.IsNone) then
                                None
                              else
                                 Some (z1.Value > z2.Value) 
    | GreaterEqExp (a1,a2) -> let z1 = execA mem a1
                              let z2 = execA mem a2
                              if (z1.IsNone || z2.IsNone) then
                                 None
                              else
                                 Some (z1.Value >= z2.Value)
    | LessExp (a1,a2)      -> let z1 = execA mem a1
                              let z2 = execA mem a2
                              if (z1.IsNone || z2.IsNone) then
                                 None
                              else
                                 Some (z1.Value < z2.Value)
    | LessEqExp (a1,a2)    -> let z1 = execA mem a1
                              let z2 = execA mem a2
                              if (z1.IsNone || z2.IsNone) then
                                None
                              else
                                 Some (z1.Value <= z2.Value)                            
    | OrExp (b1,b2)        -> let ob1 = execB mem b1
                              let ob2 = execB mem b2
                              if (ob1.IsNone || ob2.IsNone) then
                                None
                              else
                                 Some (ob1.Value || ob2.Value)
    | ShortOrExp (b1,b2)   -> let ob1 = execB mem b1
                              let ob2 = execB mem b2
                              if (ob1.IsNone || ob2.IsNone) then
                                None
                              else
                                 Some (ob1.Value || ob2.Value)                                                         
    | AndExp (b1, b2)      -> let ob1 = execB mem b1
                              let ob2 = execB mem b2
                              if (ob1.IsNone || ob2.IsNone) then
                                 None
                              else
                                 Some (ob1.Value && ob2.Value)
    | ShortAndExp (b1, b2) -> let ob1 = execB mem b1
                              let ob2 = execB mem b2
                              if (ob1.IsNone || ob2.IsNone) then
                                None
                              else
                                 Some (ob1.Value && ob2.Value)     
    | NotExp b1            -> Some (not(execB mem b1))                  

    | _                    -> None //keep or not

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
                | ArrayAssignment (var, ind, aexp) -> let z1 = execA mem ind
                                                      let z2 = execA mem aexp
                                                      let b = mem.containsKey var 
                                                      if z1.IsSome && z2.IsSome && b then
                                                         let oldA = Map.find var mem 
                                                         let newA = x.[0..ind-1]@[aexp]@ x.[ind+1..oldA.Length-1]
                                                          Some (Map.add var newA mem)
                                                      else 
                                                          None 
                | _                                -> failwith "wrong input"
    | _      -> failwith "wrong input"
    

let chooseBranch q mem edges = 
    Set.filter (fun (q1,exp,_) -> (q1 = q) && ((exec mem exp).IsSome) ) edges 

 let testExec =
     printfn "%A" (execA (Map.ofList [("x",0);("y",3)]) (Plus (Var "y", Num 5) ) )
    // printfn "%A" (exec (Map.ofList [("x",0)]) (B (ShortAndExp (GreaterEqExp (Var "x", Num 0),NotExp (ShortOrExp (EqExp (Var "x", Num 0), ShortOrExp (LessEqExp (Var "x", Num 0), FExp))))))


