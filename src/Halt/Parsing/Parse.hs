module Halt.Parsing.Parse where

import Halt.AST
import Halt.Parsing.Elements
import Halt.Parsing.Common
import Halt.Parsing.Indent
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec.Combinator
import Text.Parsec hiding (State)
import Text.Parsec.Expr
import Data.Functor.Identity

typeLiteral :: Parser TypeLiteral
typeLiteral = buildExpressionParser [[Infix (word "->" *> return Function) AssocRight]] typeTerm

-- | Function type declaration
typeTerm :: Parser TypeLiteral
typeTerm =     parens typeLiteral
       <|>     Parameter <$> (lower <* spaces)
       <|> try (Generic  <$> capitalIdentifier <*> many1 typeTerm)
       <|>     Concrete  <$> capitalIdentifier
       <|>     (word "()" *> return Unit)

-- | Variable type declaration
typeTerm' :: Parser TypeLiteral
typeTerm' = try (word "var" *> return Var)
        <|>     Concrete <$> capitalIdentifier
        <|>     parens typeTerm

assignment :: Parser Statement
assignment = Assignment <$> typeTerm' <*> (lowerIdentifier <* word "=") <*> expression

if' :: Parser Statement
if' = ifThen <*> else'
    where if''   = If <$> (word "if" *> expression)
          ifThen = if'' <*> (word "then" *> singleOrBlock statement)
          else'  = Just <$> (withIndent (word "else") *> singleOrBlock statement)

for :: Parser Statement
for = for' <*> singleOrBlock statement
    where withDyn = DynamicWithStaticBound <$> expression <*> (word "|" *> expression)
          static  = StaticBound <$> expression
          bound   = try withDyn <|> static
          for'    = For <$> (word "for"  *> lowerIdentifier)
                        <*> (word "from" *> expression)
                        <*> (word "to"   *> bound)

return' :: Parser Statement
return' = Return <$> (word "return" *> expression)

statement :: Parser Statement
statement = cases <* optional (char '\n')
    where cases = try if'
              <|> try for
              <|> return'
              <|> try assignment
              <|>     NakedExpr <$> expression

identifier' :: Parser Expression
identifier' = Identifier <$> anyIdentifier
    where anyIdentifier = (concat <$> many (try (capitalIdentifier <++> word ".")))
                     <++> (capitalIdentifier <|> lowerIdentifier)

literal :: Parser Expression
literal = try (DoubleLiteral <$> doubleLiteral)
      <|>     IntLiteral     <$> intLiteral
      <|>     StringLiteral  <$> stringLiteral

argument :: Parser Expression
argument = parens expression <|> identifier' <|> literal

functionApp :: Parser Expression
functionApp = FunctionApp <$> fn <*> many1 argument
    where fn = parens expression <|> identifier'

expressionTerm :: Parser Expression
expressionTerm =     literal
             <|> try functionApp
             <|>     identifier'

type Op = Operator String IndentLevel Identity Expression

makeOp :: String -> Op
makeOp s = Infix (word s *> return combine) AssocLeft
    where combine left right = FunctionApp (Identifier s) [left, right]

opTable :: [[Op]]
opTable = [ [ makeOp "*", makeOp "/" ]
          , [ makeOp "+", makeOp "-" ]
          , [ makeOp ">", makeOp "<" ] ]

expression :: Parser Expression
expression = buildExpressionParser opTable expressionTerm

module' :: Parser String
module' = capitalIdentifier <++> (concat <$> many (word "." <++> capitalIdentifier))

import' :: Parser Declaration
import' = Import <$> (word "import" *> module') <?> "import declaration"

importAs :: Parser Declaration
importAs = ImportAs
       <$> (word "import" *> module')
       <*> (word "as" *> capitalIdentifier)
       <?> "named import declaration"

functionType :: Parser Declaration
functionType = FunctionType
           <$> lowerIdentifier
           <*> (word "::" *> typeLiteral)
           <?> "function type declaration"

functionDecl :: Parser Declaration
functionDecl = FunctionDecl
           <$> lowerIdentifier
           <*> many1 lowerIdentifier
           <*> (word "->" *> singleOrBlock statement)
           <?> "function declaration"

data' :: Parser Declaration
data' = Data
   <$> (word "data" *> capitalIdentifier)
   <*> many (letter <* spaces)
   <*> (word "=" *> cons `sepBy1` word "|")
   <?> "data declaration"
   where cons = (,) <$> capitalIdentifier <*> many typeLiteral

record :: Parser Declaration
record = Record
     <$> (word "record" *> capitalIdentifier)
     <*> (word "=" *> singleOrBlock field)
     <?> "record declaration"
     where field = (,) <$> lowerIdentifier <*> (word "::" *> typeLiteral)

declaration :: Parser Declaration
declaration = try importAs
          <|> try import'
          <|> try data'
          <|> try record
          <|> try functionType
          <|>     functionDecl

program :: Parser [Declaration]
program = many1 (declaration <* skipMany (oneOf "\n\t "))

parseFile :: String -> IO [Declaration]
parseFile path = do
    file <- readFile path
    return $ parseHelper program file
