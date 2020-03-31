open FSharp.Text.Lexing
open System
open GCLTypesAST
open GCLParser
open GCLLexer
open PGGenerator
open GraphvizGenerator
open Interpreter

let code = IO.File.ReadAllText "../../../code.gc"

let parse input =
     // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res
    
    
[<EntryPoint>]
let main argv =
    let isDet = List.contains "-d" (Array.toList argv)

    try
        let ast = parse code
        let edges = EdgesC (Node 0) EndNode Set.empty isDet ast
//        let graphviz = generateGraphviz (Set.toList edges)
        let state = GetInitVars (C ast)
                    |> RunPG (Node 0) edges
        PrintState state
    with
        err -> printfn "An error has occured"
               printfn "%A" err

    0 //return