module MA1.Signs

open GCLTypesAST

let AddSigns = function
    | (Plus, Plus)
    | (Zero, Plus)
    | (Plus, Zero) -> set [ Plus ]
    | (Minus, Minus)
    | (Zero, Minus)
    | (Minus, Zero) -> set [ Minus ]
    | (Plus, Minus)
    | (Minus, Plus) -> set [ Plus; Zero; Minus ]
    | (Zero, Zero) -> set [ Zero ]

let SubSigns = function
    | (Zero, Minus)
    | (Plus, Minus)
    | (Plus, Zero) -> set [ Plus ]
    | (Zero, Plus)
    | (Minus, Plus)
    | (Minus, Zero) -> set [ Minus ]
    | (Plus, Plus)
    | (Minus, Minus) -> set [ Plus; Zero; Minus ]
    | (Zero, Zero) -> set [ Zero ]

let MulSigns = function
    | (Plus, Plus)
    | (Minus, Minus) -> set [ Plus ]
    | (Plus, Minus)
    | (Minus, Plus) -> set [ Minus ]
    | _ -> set [ Zero ]

let DivSigns = function
    | (Zero, Plus)
    | (Zero, Minus) -> set [ Zero ]
    | (Plus, Plus)
    | (Minus, Minus) -> set [ Plus ]
    | (Plus, Minus)
    | (Minus, Plus) -> set [ Minus ]
    | (Plus, Zero)
    | (Zero, Zero)
    | (Minus, Zero) -> failwith "Undefined behaviour: Division by zero"

let PowSigns = function
    | (Plus, Plus)
    | (Plus, Zero)
    | (Minus, Zero) -> set [ Plus ]
    | (Zero, Plus) -> set [ Zero ]
    | (Minus, Plus) -> set [ Minus ]
    | (Zero, Zero) -> failwith "Undefined behaviour: Zero to the power of zero" // investigate
    | (Zero, Minus)
    | (Minus, Minus)
    | (Plus, Minus) -> failwith "Undefined behaviour: Exponent cannot be negative"

let NegSigns = function
    | Plus -> set [ Minus ]
    | Minus -> set [ Plus ]
    | Zero -> set [ Zero ]

let EqSigns = function
    | (Zero, Zero) -> set [ TrueSign ]
    | (Plus, Plus)
    | (Minus, Minus) -> set [ TrueSign; FalseSign ]
    | _ -> set [ FalseSign ]

let NeqSigns = function
    | (Zero, Zero) -> set [ FalseSign ]
    | (Plus, Plus)
    | (Minus, Minus) -> set [ TrueSign; FalseSign ]
    | _ -> set [ TrueSign ]

let GrSigns = function
    | _ -> failwith "missing implementation of GrSigns"

let GrEqSigns = function
    | _ -> failwith "missing implementation of GrEqSigns"

let LsSigns = function
    | _ -> failwith "missing implementation of LsSigns"

let LsEqSigns = function
    | _ -> failwith "missing implementation of LsEqSigns"

let OrSigns = function
    | _ -> failwith "missing implementation of OrSigns"

let AndSigns = function
    | (TrueSign, TrueSign) -> set [ TrueSign ]
    | _ -> set [ FalseSign ]

let SCOrSigns = function
    | _ -> failwith "missing implementation of SCOrSigns"

let SCAndSigns = function
    | _ -> failwith "missing implementation of SCAndSigns"

let NotSigns = function
    | TrueSign -> set [ FalseSign ]
    | FalseSign -> set [ TrueSign ]
