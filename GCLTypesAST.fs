module GCLTypesAST

// Arithmetic expressions
type AExp = 
      | Num of int 
      | Var of string
      | Arr of (string*AExp)
      | Add of (AExp*AExp) 
      | Sub of (AExp*AExp)
      | Mul of (AExp*AExp)
      | Div of (AExp*AExp)
      | Neg of AExp
      | Pow of (AExp*AExp)

// Boolean expressions
type BExp =
      | TExp
      | FExp
      | And of (BExp*BExp)
      | Or of (BExp*BExp)
      | SCAnd of (BExp*BExp)  // short-circuit and
      | SCOr of (BExp*BExp)   // short-circuit or
      | Not of BExp
      | Eq of (AExp*AExp)
      | NEq of (AExp*AExp)
      | Gr of (AExp*AExp)
      | GrEq of (AExp*AExp)
      | Ls of (AExp*AExp)
      | LsEq of (AExp*AExp)

// Command expression
type CExp = 
      | Asgmt of (string*AExp)
      | ArrAsgmt of (string*AExp*AExp)
      | Skip
      | CSeq of (CExp*CExp)
      | If of GCExp
      | Do of GCExp

// Guarded command expression
and GCExp = 
      | GC of (BExp*CExp)
      | GCSeq of (GCExp*GCExp)
        
// General expression
type Exp =
    | A of AExp
    | B of BExp
    | C of CExp
    | G of GCExp
   
// Node type
type NodeType =
    | EndNode
    | Node of int

// Edge
type Edge = NodeType * Exp * NodeType

// Memory
type Memory = Map<string, int>

// State of the program after execution
type ProgramState = 
    | Stuck      of NodeType * Memory
    | Terminated of Memory

// Converts a Node to string
let ToStringNode = function
    | Node 0    -> "q▷"
    | EndNode   -> "q◀"
    | Node n    ->  sprintf "q%d" n
