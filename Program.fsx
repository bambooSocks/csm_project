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

let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res
let code = System.IO.File.ReadAllText "./code.gc"

// printfn "%A" code

try
    printfn "%A" (parse code)    
with
    err -> printfn "An error has occured"
           printfn "%A" err
