{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Halt.Parsing.Common where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding (token, State)
import Data.Functor.Identity
import Halt.Utility

type CharStream s m = Stream s m Char
