module Halt.Parse where

import Halt.AST
import Control.Applicative hiding ((<|>))
import Data.Monoid
import Text.Parsec hiding (token, State)
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Indent
import Control.Monad.Trans.State

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

monoidFromMaybe :: Monoid a => Maybe a -> a
monoidFromMaybe Nothing  = mempty
monoidFromMaybe (Just a) = a

style :: GenLanguageDef String () (State SourcePos)
style = emptyDef { commentStart    = "{-"
                 , commentEnd      = "-}"
                 , commentLine     = "--"
                 , nestedComments  = True
                 , identStart      = letter
                 , identLetter     = alphaNum <|> oneOf "_'"
                 , opStart         = opLetter style
                 , opLetter	       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                 , reservedOpNames = []
                 , reservedNames   = []
                 , caseSensitive   = True }

type Parser a = IndentParser String () a

parser :: GenTokenParser String () (State SourcePos)
parser = makeTokenParser style

iParse :: Parser a -> SourceName -> String -> Either ParseError a
iParse aParser sourceName' input = runIndent sourceName' $ runParserT aParser () sourceName' input

parseHelper :: Parser a -> String -> a
parseHelper pars str = case iParse pars "" str of
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
if' = ifThen <*> else'
    where if''   = (If <$> (word "if" *> expression))
          ifThen = withBlock ($) if'' statement
          else'  = (optionMaybe $ withBlock' (word "else") statement)

for :: Parser Statement
for = withBlock ($) for' statement
    where withDyn = DynamicWithStaticBound <$> expression <*> (word "|" *> expression)
          static  = StaticBound <$> expression
          bound   = try withDyn <|> static
          for'    = For <$> (word "for" *> identifier parser)
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

functionApp :: Parser Expression
functionApp = FunctionApp <$> expression <*> many1 expression

expression :: Parser Expression
expression = try (DoubleLiteral <$> float parser)
         <|> IntLiteral <$> integer parser
         <|> StringLiteral <$> stringLiteral parser
         <|> Identifier <$> identifier parser
         <|> functionApp
