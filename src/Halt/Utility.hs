module Halt.Utility where

import Control.Applicative

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
