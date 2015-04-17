{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Halt.Parsing.Common where

import Text.Parsec hiding (token, State)

type CharStream s m = Stream s m Char
