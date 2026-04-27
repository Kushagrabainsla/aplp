{-
  Module: AST
  Description: Defines the Abstract Syntax Tree and Value types for the language.
  Design Decisions:
    - Values include Int, Bool, and recursive Lists to support the core and list requirements.
    - Closures (FunVal) are used to implement first-class functions, allowing for sound lexical scoping.
    - Patterns are separate from Expressions to simplify the match logic.
-}

module AST where

-- Binary operators supported by the language
data Binop = Plus | Minus | Times | Divide | Eq | Lt | Gt | Le | Ge | And | Or
  deriving (Show, Eq)

-- Patterns used in 'match' expressions
data Pattern
  = PVar String       -- Matches any value and binds it to a name
  | PInt Int          -- Matches a literal integer
  | PBool Bool        -- Matches a literal boolean
  | PListNil          -- Matches an empty list []
  | PCons String String -- Matches a non-empty list (h:t) and binds head and tail
  | PWildcard         -- Matches anything ( _ )
  deriving (Show, Eq)

-- Abstract Syntax Tree for expressions
data Expr
  = ValExpr Value              -- Literal values
  | VarExpr String             -- Variable access
  | OpExpr Binop Expr Expr     -- Binary operations
  | IfExpr Expr Expr Expr      -- Conditionals
  | SeqExpr Expr Expr          -- Sequencing (e1; e2)
  | AssignExpr String Expr     -- Variable assignment
  | CallExpr String [Expr]     -- Function call
  | MatchExpr Expr [(Pattern, Expr)] -- Pattern matching
  | ListExpr [Expr]            -- List literal [e1, e2, ...]
  | ConsExpr Expr Expr         -- Cons operation (e1 : e2)
  deriving (Show, Eq)

-- Values that expressions evaluate to
data Value
  = IntVal Int
  | BoolVal Bool
  | ListVal [Value]
  | FunVal [String] Expr       -- Parameters and body (Closures are handled in the interpreter)
  | NilVal                     -- Representing empty list or unit
  deriving (Show, Eq)

-- A Program consists of top-level function definitions and a final expression
data Definition = FunDef String [String] Expr
  deriving (Show, Eq)

data Program = Program [Definition] Expr
  deriving (Show, Eq)
