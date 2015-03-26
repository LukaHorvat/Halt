{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Halt.Parsing.Common where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding (token, State)
{-
    import Text.Parsec.Expr
    import Text.Parsec.String
-}
import Data.Functor.Identity

type CharStream s = Stream s Identity Char

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

parseHelper :: Stream s Identity t => Parsec s () a -> s -> a
parseHelper pars str = case parse pars "" str of
    Left err -> error $ show err
    Right a  -> a
