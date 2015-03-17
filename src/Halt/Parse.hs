module Halt.Parse where

import Halt.AST
import Control.Applicative hiding ((<|>))
import Data.Monoid
import Text.Parsec hiding (token)
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

monoidFromMaybe :: Monoid a => Maybe a -> a
monoidFromMaybe Nothing  = mempty
monoidFromMaybe (Just a) = a

parser = makeTokenParser haskellStyle

parseHelper :: Parser a -> String -> a
parseHelper pars str = case parse pars "" str of
    Left err -> error $ show err
    Right a  -> a

word :: String -> Parser String
word = symbol parser

token :: Parser a -> Parser a
token pars = pars <* whiteSpace parser

typeLiteral :: Parser TypeLiteral
typeLiteral = buildExpressionParser [[Infix (word "->" *> return Function) AssocRight]] typeTerm

typeIdentifier :: Parser String
typeIdentifier = upper <:> identifier parser

typeTerm :: Parser TypeLiteral
typeTerm = parens parser typeLiteral
       <|> Parameter <$> token lower
       <|> try (Generic <$> typeIdentifier <*> many1 typeTerm)
       <|> Concrete <$> typeIdentifier

typeTerm' :: Parser TypeLiteral
typeTerm' = try (word "var" *> return Var) <|> typeTerm

assignment :: Parser Statement
assignment = Assignment <$> typeTerm' <*> identifier parser <* word "=" <*> expression

if' :: Parser Statement
if' = If <$> (word "if" *> expression) <*> statement <*> optionMaybe (word "else" *> statement)

for :: Parser Statement
for = For <$> (word "for" *> identifier parser)
          <*> (word "from" *> expression)
          <*> (word "to" *> bound)
          <*> statement
          where bound = Bound <$> expression <*> optionMaybe (word "|" *> expression)

return' :: Parser Statement
return' = Return <$> (word "return" *> expression)

statement :: Parser Statement
statement = if'
        <|> for
        <|> return'
        <|> assignment

functionApp :: Parser Expression
functionApp = FunctionApp <$> expression <*> many1 expression

expression :: Parser Expression
expression = functionApp
