open FSharp.Text.Lexing
open System
open GCLTypesAST
open GCLParser
open GCLLexer
open PGGenerator
open GraphvizGenerator
open Interpreter

let code = System.IO.File.ReadAllText "../../../code.gc"

let parse input =
     // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res
    
    
[<EntryPoint>]
let main argv =
    let getDet = List.contains "-d" (Array.toList argv)  
    let ast = parse code
    let edges = edgesC (Node 0) EndNode Set.empty getDet ast
    let graphviz = generateGraphviz (Set.toList edges)
    let branch = chooseBranch (Node 0) (Map.ofList [("x",1);("y",2)]) edges
    let testExec = execB (Map.ofList [("x",0);("y",3)]) (NotExp (LessExp(Num 1, Num 2)) )
    
    try
        // printfn "%A" ast
        printfn "%A" testExec
        // printfn "%A" graphviz
        //printfn "%A" branch
    with
        err -> printfn "An error has occured"
               printfn "%A" err
    
    //return
    0