module Halt.Parsing.Indent where

import Halt.AST
import Halt.Parsing.Common
import Halt.Parsing.Elements
import Control.Applicative hiding ((<|>), many, optional)
import Data.Monoid
import Control.Monad.State.Class
import Control.Monad.State
import Text.Parsec hiding (token, State)

type IndentLevel = Int
type Parser a = ParsecT String IndentLevel (State IndentLevel) a

parseHelper :: Parser a -> String -> a
parseHelper pars str = case evalState (runParserT pars 0 "" str) 0 of
    Left err -> error $ show err
    Right a  -> a

singleOrBlock :: Parser a -> Parser [a]
singleOrBlock p = (return <$> p) <|> block
    where block = modify (+ 1) *> many1 (withIndent p) <* modify (subtract 1)
          withIndent p' = try $ (char '\n' >> get >>= (`count` char '\t')) *> p'
