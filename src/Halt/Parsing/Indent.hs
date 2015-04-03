module Halt.Parsing.Indent where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Data.Functor.Identity
import Text.Parsec hiding (token, State)

type IndentLevel = Int
type Parser a = ParsecT String IndentLevel Identity a

parseHelper :: Parser a -> String -> a
parseHelper pars str = case runParser pars 0 "" str of
    Left err -> error $ show err
    Right a  -> a

withIndent :: Parser a -> Parser a
withIndent p = try (getState >>= indents) *> p

indent :: Parser ()
indent = void (char '\t') <|> void (count 4 (char ' ')) <?> "indentation"

indents :: Int -> Parser ()
indents n = void (count n indent) <?> (show n) ++ " indentation levels"

indented :: Parser a -> Parser a
indented p = try $ modifyState (+ 1) *> p <* modifyState (subtract 1)

singleOrBlock :: Parser a -> Parser [a]
singleOrBlock p = (char '\n' *> block) <|> (return <$> p)
    where block = indented $ many1 (withIndent p) --TODO: Parsing empty lines
