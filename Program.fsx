#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer
#load "PGGenerator.fs"
open PGGenerator
#load "GraphvizGenerator.fs"
open GraphvizGenerator
#load "Interpreter.fs"
open Interpreter

let programArgs = fsi.CommandLineArgs |> Array.toList

let getDet = List.contains "-d" programArgs   

let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res
let code = System.IO.File.ReadAllText "./code.gc"
let ast = parse code
let edges = edgesC (Node 0) EndNode Set.empty getDet ast
let graphviz = generateGraphviz (Set.toList edges)
let branch = chooseBranch (Node 0) (Map.ofList [("x",1);("y",2)]) edges
let testExec = execA (Map.ofList [("x",0);("y",3)]) (Plus (Var "y", Var "x") ) 

try
    // printfn "%A" ast
    printfn "%A" testExec
    // printfn "%A" graphviz
    //printfn "%A" branch
with
    err -> printfn "An error has occured"
           printfn "%A" err
