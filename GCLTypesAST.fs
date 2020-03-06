module GCLTypesAST

type AExp = 
      | Num of int 
      | Var of string
      | Array of (string*AExp)
      | Plus of (AExp*AExp) 
      | Minus of (AExp*AExp)
      | Mul of (AExp*AExp)
      | Div of (AExp*AExp)
      | UnaryMinus of (AExp)
      | Pow of (AExp*AExp)

type BExp =
      | TExp
      | FExp
      | AndExp of (BExp*BExp)
      | OrExp of (BExp*BExp)
      | ShortAndExp of (BExp*BExp)
      | ShortOrExp of (BExp*BExp)
      | NotExp of BExp
      | EqExp of (BExp*BExp)
      | NotEqExp of (BExp*BExp)
      | GreaterExp of (BExp*BExp)
      | GreaterEqExp of (BExp*BExp)
      | LessExp of (BExp*BExp)
      | LessEqExp of (BExp*BExp)

type CExp = 
      | Assignment of (string*AExp)
      | ArrayAssignment of (string*AExp*AExp)
      | SkipExp
      | CExpSeq of (CExp*CExp)
      | IfExp of GCExp
      | DoExp of GCExp

   and 

type GCExp = 
      | GC of (BExp*CExp)
      | GCSeq of (GCExp*GCExp)
        
