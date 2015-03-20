module Halt.AST where

data Declaration = --     module
                   Import String
                   --       module alias
                 | ImportAs String String
                   --           name   type
                 | FunctionType String TypeLiteral
                   --           name   args     body
                 | FunctionDecl String [String] Statement
                   --   name   generics    cases
                   --                 constr    types
                 | Data String [Char] [(String, [TypeLiteral])]
                   --     name        fields
                   --            fieldName fieldType
                 | Record String [(String, TypeLiteral)]
                 deriving Show

data TypeLiteral = --        typeParam
                   Parameter Char
                   --       name
                 | Concrete String
                   --      outer  generics
                 | Generic String [TypeLiteral]
                   --       from        to
                 | Function TypeLiteral TypeLiteral
                 | Var
                 deriving Show

data Statement = --         type        name   value
                 Assignment TypeLiteral String Expression
                 -- condition  then      else
               | If Expression [Statement] (Maybe [Statement])
                 --  var    start      bound body
               | For String Expression Bound [Statement]
                 --     value
               | Return Expression
                 --        expr
               | NakedExpr Expression
               deriving Show

data Bound = --          bound
             StaticBound Expression
             --                     dynamic    static
           | DynamicWithStaticBound Expression Expression
           deriving Show

data Expression = --          function   arguments
                  FunctionApp Expression [Expression]
                | IntLiteral Integer
                | DoubleLiteral Double
                | StringLiteral String
                | Identifier String
                deriving Show
