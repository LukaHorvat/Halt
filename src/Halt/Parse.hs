module Halt.Parse where

import Halt.AST
import Control.Applicative ((<*), (*>), (<$>), liftA2, Applicative)
import Data.Monoid
import Text.Parsec
import Text.Parsec.String

withSpaces :: Parser a -> Parser a
withSpaces parser = parser <* skipMany (space <|> newline)

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

monoidFromMaybe :: Monoid a => Maybe a -> a
monoidFromMaybe Nothing  = mempty
monoidFromMaybe (Just a) = a

int :: Parser Int
int = withSpaces $ read <$> many digit

double :: Parser Double
double = withSpaces $ read <$> many1 digit <++> option "" (string "." <++> many1 digit)

stringLiteral :: Parser String
stringLiteral = withSpaces $ char '"' *> many (noneOf "\"") <* char '"'

parseHelper :: Parser a -> String -> a
parseHelper parser str = case parse parser "" str of
    Left err -> error $ show err
    Right a  -> a
