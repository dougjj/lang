module Language where 

type Number = Integer

data BinaryOp
    = Plus | Minus | Times | Divide
    | LogicalAnd | LogicalOr
    | Leq | Geq | Lt | Gt | Equals | NotEquals
    deriving (Show, Eq)

data UnaryOp = Not | UnaryMinus
    deriving (Show, Eq)

data Expression
    = Variable String
    | Number Number
    | BinaryOp BinaryOp Expression Expression
    | UnaryOp UnaryOp Expression
    | Function String [Expression]
    deriving (Show, Eq)

type FreeVariables = [String]

-- | A Statement is either an assignment, an if else clause, print, input, semicolon, a function definition. 
-- data FunctionDef = FunctionDef String [String] Statement

data Statement
     = Assignment String Expression
     | IfThenElse Expression Statement Statement
     | Print Expression
     | Input String
     | Semicolon Statement Statement
     | FunctionAssignment String [String] Statement
     deriving (Show, Eq)

type Block = [Statement]