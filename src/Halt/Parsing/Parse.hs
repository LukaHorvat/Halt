module Halt.Parsing.Parse where

import Halt.AST
import Halt.Parsing.Elements
import Halt.Parsing.Common
import Control.Applicative hiding ((<|>), many)
import Data.Monoid
import Text.Parsec hiding (token, State)
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Indent
import Control.Monad.Trans.State

type Parser a = IndentParser String () a

typeLiteral :: Parser TypeLiteral
typeLiteral = buildExpressionParser [[Infix (word "->" *> return Function) AssocRight]] typeTerm

-- | Function type declaration
typeTerm :: Parser TypeLiteral
typeTerm = parens typeLiteral
       <|> Parameter <$> (lower <* spaces)
       <|> try (Generic <$> capitalIdentifier <*> many1 typeTerm)
       <|> Concrete <$> capitalIdentifier

-- | Variable type declaration
typeTerm' :: Parser TypeLiteral
typeTerm' = try (word "var" *> return Var)
        <|> Concrete <$> capitalIdentifier
        <|> parens typeTerm

assignment :: Parser Statement
assignment = Assignment <$> typeTerm' <*> (lowerIdentifier <* word "=") <*> expression

if' :: Parser Statement
if' = ifThen <*> else'
    where if''   = If <$> (word "if" *> expression)
          ifThen = withBlock ($) if'' statement
          else'  = optionMaybe $ withBlock' (word "else") statement

for :: Parser Statement
for = withBlock ($) for' statement
    where withDyn = DynamicWithStaticBound <$> expression <*> (word "|" *> expression)
          static  = StaticBound <$> expression
          bound   = try withDyn <|> static
          for'    = For <$> (word "for" *> lowerIdentifier)
                        <*> (word "from" *> expression)
                        <*> (word "to" *> bound)

return' :: Parser Statement
return' = Return <$> (word "return" *> expression)

statement :: Parser Statement
statement = try if'
        <|> try for
        <|> try return'
        <|> try assignment
        <|> NakedExpr <$> expression

identifier' :: Parser Expression
identifier' = Identifier <$> lowerIdentifier

identOrParens :: Parser Expression
identOrParens = parens expression <|> identifier'

functionApp :: Parser Expression
functionApp = FunctionApp <$> identOrParens <*> many1 identOrParens

expressionTerm :: Parser Expression
expressionTerm = try (DoubleLiteral <$> doubleLiteral)
             <|> IntLiteral <$> intLiteral
             <|> StringLiteral <$> stringLiteral parser
             <|> try functionApp
             <|> Identifier <$> lowerIdentifier

type Op = Operator String () (State SourcePos) Expression

makeOp :: String -> Op
makeOp s = Infix (word s *> return combine) AssocLeft
    where combine left right = FunctionApp (Identifier s) [left, right]

opTable :: [[Op]]
opTable = [[ makeOp "*", makeOp "/" ]
          ,[ makeOp "+", makeOp "-" ]]

expression :: Parser Expression
expression = buildExpressionParser opTable expressionTerm

module' :: Parser String
module' = capitalIdentifier <++> (concat <$> many (word "." <++> capitalIdentifier))

import' :: Parser Declaration
import' = Import <$> (word "import" *> module')

importAs :: Parser Declaration
importAs = ImportAs <$> (word "import" *> module') <*> (word "as" *> capitalIdentifier)

functionType :: Parser Declaration
functionType = FunctionType <$> lowerIdentifier <*> (word "::" *> typeLiteral)

functionDecl :: Parser Declaration
functionDecl = FunctionDecl
           <$> lowerIdentifier
           <*> many1 lowerIdentifier
           <*> (word "->" *> statement)

data' :: Parser Declaration
data' = Data
   <$> (word "data" *> capitalIdentifier)
   <*> many (letter <* spaces)
   <*> (word "=" *> cons `sepBy1` word "|")
   where cons = (,) <$> capitalIdentifier <*> many typeLiteral

record :: Parser Declaration
record = Record
     <$> (word "record" *> capitalIdentifier)
     <*> (withBlock' (word "=") field)
     where field = (,) <$> lowerIdentifier <*> (word "::" *> typeLiteral)

--Text.Parsec.Token eats whitespace too agressively. Rethink
