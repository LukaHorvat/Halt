{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Halt.Parsing.Common where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding (token, State)
{-
    import Text.Parsec.Expr
    import Text.Parsec.String
-}
import Data.Functor.Identity
import Control.Monad.State

type CharStream s m = Stream s m Char

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
