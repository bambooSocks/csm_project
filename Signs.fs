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
    | (Minus, Plus) -> set [ Minus;Plus ] //investigate
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
    | (Zero,Zero) 
    | (Minus,Zero)
    | (Minus,Plus) -> set [FalseSign]
    | (Minus,Minus)
    | (Plus,Plus) -> set [FalseSign;TrueSign]
    | _ -> set [ TrueSign ]

let GrEqSigns = function
    | (Minus,Minus)
    | (Plus,Plus) -> set [TrueSign;FalseSign]
    | (Minus,Plus)
    | (Minus,Zero)
    | (Zero,Plus) -> set [FalseSign]
    | _ -> set [TrueSign]

let LsSigns  = function
    | (Minus,Zero)
    | (Minus,Plus) -> set [TrueSign]
    | (Minus,Minus)
    | (Plus,Plus) -> set [FalseSign;TrueSign]
    | _ -> set [FalseSign]

let LsEqSigns = function
    | (Minus,Minus)
    | (Plus,Plus) -> set [TrueSign;FalseSign]
    | (Minus,Plus)
    | (Minus,Zero)
    | (Zero,Plus) -> set [TrueSign]
    | _ -> set [FalseSign]

let OrSigns = function
    | (FalseSign,FalseSign) -> set [FalseSign]
    | _ -> set [TrueSign]

let AndSigns = function
    | (TrueSign, TrueSign) -> set [ TrueSign ]
    | _ -> set [ FalseSign ]

let SCOrSigns = function
    | (TrueSign,_) -> set [TrueSign]
    | (FalseSign,FalseSign) -> set [FalseSign]
    | _ -> set [TrueSign]
    
let SCAndSigns = function
    | (FalseSign,_) -> set [FalseSign]
    | (TrueSign,TrueSign) -> set [TrueSign]
    | _ -> set [FalseSign]
let NotSigns = function
    | TrueSign -> set [ FalseSign ]
    | FalseSign -> set [ TrueSign ]
    