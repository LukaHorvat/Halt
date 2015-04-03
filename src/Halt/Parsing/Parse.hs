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
import Control.Monad

typeLiteral :: Parser TypeLiteral
typeLiteral = buildExpressionParser [[Infix (word "->" *> return Function) AssocRight]] typeTerm
          <?> "function type"

-- | Function type declaration
typeTerm :: Parser TypeLiteral
typeTerm = try (word "()" *> return Unit <?> "()")
       <|>     parens typeLiteral
       <|>     (Parameter <$> (lower <* spaces) <?> "type parameter")
       <|> try (Generic  <$> capitalIdentifier <*> many1 typeTerm <?> "generic type")
       <|>     (Concrete  <$> capitalIdentifier <?> "concrete type")
       <?> "type term"

-- | Variable type declaration
typeTerm' :: Parser TypeLiteral
typeTerm' = try (word "var" *> return Var <?> "var")
        <|>     (Concrete <$> capitalIdentifier <?> "concrete type")
        <|>     parens typeLiteral
        <?> "type"

assignment :: Parser Statement
assignment = Assignment
         <$> typeTerm' <*> (lowerIdentifier <* word "=") <*> expression <?> "assignments"

if' :: Parser Statement
if' = do
    cond <- word "if" *> expression
    isMulti <- word "then" *> optionMaybe (lookAhead $ char '\n')
    thenBlock <- singleOrBlock statement
    elseBlock <- optionMaybe $ do
        --if the then block is on the same line, then there is no indentation before 'else'
        void $ case isMulti of Nothing -> word "else"
                               _       -> withIndent (word "else")
        singleOrBlock statement
    return $ If cond thenBlock elseBlock

for :: Parser Statement
for = for' <*> singleOrBlock statement <?> "for statement"
    where withDyn = DynamicWithStaticBound <$> expression <*> (word "|" *> expression)
          static  = StaticBound <$> expression
          bound   = try withDyn <|> static
          for'    = For <$> (word "for"  *> lowerIdentifier)
                        <*> (word "from" *> expression)
                        <*> (word "to"   *> bound)

return' :: Parser Statement
return' = Return <$> (word "return" *> expression) <?> "return statement"

statement :: Parser Statement
statement = cases <* optional (char '\n') <?> "statement"
    where cases = try if'
              <|> try for
              <|>     return'
              <|> try assignment
              <|>     (NakedExpr <$> expression <?> "naked expression")

keywords :: [String]
keywords = ["if", "then", "else", "for", "from", "to", "return", "var"]

identifier' :: Parser Expression
identifier' = anyIdentifier >>= notKeyword <?> "any indentifier"
    where anyIdentifier = (concat <$> many (try (capitalIdentifier <++> word ".")))
                     <++> (capitalIdentifier <|> lowerIdentifier)
          notKeyword ident | ident `notElem` keywords = return $ Identifier ident
                           | otherwise                = fail message
                           where message = "keyword " ++ ident ++ " cannot be used as an identifier"

literal :: Parser Expression
literal = try (DoubleLiteral <$> doubleLiteral)
      <|>     IntLiteral     <$> intLiteral
      <|>     StringLiteral  <$> stringLiteral

argument :: Parser Expression
argument = parens expression <|> identifier' <|> literal

functionApp :: Parser Expression
functionApp = FunctionApp <$> fn <*> many1 (try argument) <?> "function application"
    where fn = try (parens operator') <|> try (parens expression) <|> identifier'
          operator' = Identifier <$> operator

expressionTerm :: Parser Expression
expressionTerm =     literal
             <|> try functionApp
             <|>     identifier'
             <?> "expression term"

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
     where field = (,) <$> lowerIdentifier <*> (word "::" *> typeLiteral <* optional (char '\n'))

declaration :: Parser Declaration
declaration = try importAs
          <|> try import'
          <|> try data'
          <|> try record
          <|> try functionType
          <|>     functionDecl

program :: Parser [Declaration]
program = many1 (declaration <* skipMany (oneOf "\n\t "))
        --FIXME: Parsing empty lines is potentially broken

parseFile :: String -> IO [Declaration]
parseFile path = do
    file <- readFile path
    return $ parseHelper program file

basicAST :: IO [Declaration]
basicAST = parseFile "./examples/basic-structure/basic.halt"
