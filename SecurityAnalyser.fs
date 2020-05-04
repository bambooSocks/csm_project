module SecurityAnalyser
open GLCTypesAST
open System.Text.RegularExpressions
open Signs
//does not account for spaces after matching the pattern in RegexMatch (but does allow several spaces in between "<" in the pattern)
let rec InputSecurityLattice input =
        printf "Please specify a security lattice: " 
        match System.Console.ReadLine() with
        | RegexMatch "^(([A-Z]|[a-z])([A-Z]|[a-z]|[0-9]))( < *([A-Z]|[a-z])([A-Z]|[a-z]|[0-9])*)*$" m -> m.Value
                                                                                                         |> fun str -> str.Trim ()
                                                                                                         |> fun str -> str.Split '<'
                                                                                                         |> Array.toList
                                                                                                                                                   
        | _ -> printfn "Not a valid lattice. Note that a lattice cannot start with a number."
               InputSecurityLattice input


let rec InputSecurityClassification input lattice: (string*string) list =
        printf "Please enter an initial value for %s: " input
        let prompt = System.Console.ReadLine()
        if List.contains prompt lattice
        then [(input,prompt)]
        else  printfn "Wrong input! Input did not match a lattice field"
              InputSecurityClassification input lattice
          
          
    
    