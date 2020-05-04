open FSharp.Text.Lexing
open System
open GCLTypesAST
open GCLParser
open GCLLexer
open PGGenerator
open GraphvizGenerator
open Interpreter
open Signs
open SignAnalyzer

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
    let argList = Array.toList argv
    let isDet = List.contains "-d" argList 
    let showGraph = List.contains "--pg" argList
    let runSignAnalysis = List.contains "-s" argList

    try
        let ast = parse code
        let edges = EdgesC (Node 0) EndNode Set.empty isDet ast
        if showGraph then
            let graphviz = GenerateGraphviz edges
            printf "%A" graphviz
        else if runSignAnalysis  then
            let initMem = GetInitSignVarsArrs (C ast)
            let initAnalysis = initAnalysis edges initMem            
            let finalAnalysis = RunSignAnalysisOnPG (set[Node 0]) initAnalysis edges
//            printf "%A" finalAnalysis
            PrintAbstractMemories (Map.find EndNode finalAnalysis)
        else
            let state = GetInitVars (C ast)
                        |> RunPG (Node 0) edges
            PrintState state
    with
        err -> printfn "An error has occured"
               printfn "%A" err

    0 //return