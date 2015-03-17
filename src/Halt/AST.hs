module Halt.AST where

data Statement = --     module
                 Import String
                 --       module alias
               | ImportAs String String
                 --            name     type
               | FunctionType String TypeLiteral
                 --            name    args      body
               | FunctionDecl String [String] Expression
                 --    name generics        cases
                 --                   constr      types
               | Data String [Char] [(String, [TypeLiteral])]
                 --      name          fields
                 --            fieldName  fieldType
               | Record String [(String, TypeLiteral)]

data TypeLiteral = --      typeParam
                   Parameter Char
                   --        name
                 | Concrete String
                   --     wrapper   generics
                 | Generic String [TypeLiteral]
                   --          from         to
                 | Function TypeLiteral TypeLiteral
                 | Var

data Expression = --             type     name    value
                  Assignment TypeLiteral String Expression
                  --  condition thenBlock      elseBlock
                | If Expression Expression (Maybe Expression)
                  --           function    arguments
                | FunctionApp Expression [Expression]
                  --   var      start       end       bound     body
                | For String Expression Expression Expression Expression
                  --        value
                | Return Expression
                  --        exps
                | Block [Expression]
                | IntLiteral Int
                | DoubleLiteral Double
                | StringLiteral String
