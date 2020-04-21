module MA1.SignAnalyser
open GCLTypesAST
open MA1
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
        | Arr (v, a)   -> if (Set.intersect (SignAnalyseA (m1,m2) a) (set [Zero;Plus])) = Set.empty
                          then Set.empty
                          else match Map.tryFind (sprintf "%s[%u]" var indVal.Value) m2 with 
                                 | Some s -> set [s]
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
                            
let SignAnalyse (m1: AbstractVariableMemory, m2: AbstractArrayMemory) exp =
    match exp with
    | B bexp -> match (SignAnalyseB (m1, m2) bexp) with
                | Some true -> Some (m1, m2)
                | _         -> None
    | C cexp -> match cexp with
                | Skip                      -> Some (m1, m2)
                | Asgmt (var, aexp)         -> match (SignAnalyseA (m1, m2) aexp) with
                                                   | Some n when mem.ContainsKey var -> Some (Map.add var n mem)
                                                   | _                               -> None
                | ArrAsgmt (var, ind, aexp) -> match (SignAnalyseA (m1, m2) ind), (SignAnalyseA (m1, m2) aexp) with
                                                   | Some z1 , Some z2 -> let varName = sprintf "%s[%i]" var z1         
                                                                          if mem.ContainsKey varName then
                                                                              Some (Map.add varName z2 mem)
                                                                          else
                                                                              None
                                                   | _                 -> None
                | _                         -> failwith "wrong input"
    | _      -> failwith "wrong input"