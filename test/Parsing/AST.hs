{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Parsing.AST where

import Test.QuickCheck
import Halt.AST
import Halt.Utility
import Control.Applicative
import Data.Char
import Data.List
import Halt.Printing.Pretty

validNames :: [String]
validNames = ["apple", "pear", "banana", "pineapple", "grape", "lemon", "orange", "tangerine"]

capitalLetter :: Gen Char
capitalLetter = elements ['A'..'Z']

lowerLetter :: Gen Char
lowerLetter = toLower <$> capitalLetter

identifierSymbol :: Gen Char
identifierSymbol = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['\'', '_']

capitalize :: String -> String
capitalize (x : xs) = toUpper x : xs

capitalIdentifier :: Gen String
--capitalIdentifier = (:) <$> capitalLetter <*> (resize 5 $ listOf identifierSymbol)
capitalIdentifier = concat <$> (resize 2 $ listOf1 $ capitalize <$> elements validNames)

lowerIdentifier :: Gen String
--lowerIdentifier = (:) <$> lowerLetter <*> (resize 5 $ listOf identifierSymbol)
lowerIdentifier = elements validNames <++>
    (concat <$> (resize 1 $ listOf $ capitalize <$> elements validNames))

rightCapitalIdentifier :: Gen String
rightCapitalIdentifier = intercalate "." <$> (resize 3 $ listOf1 capitalIdentifier)

rightLowerIdentifier :: Gen String
rightLowerIdentifier = (resize 2 $ rightCapitalIdentifier) <++> return "." <++> lowerIdentifier

rightIdentifier :: Gen String
rightIdentifier = oneof [rightCapitalIdentifier, rightLowerIdentifier]

positiveNum :: (Arbitrary a, Num a, Ord a) => Gen a
positiveNum = (\(Positive n) -> n) <$> arbitrary

data ExpressionVariant = NonLiteral | WithLiterals

expression' :: ExpressionVariant -> Gen Expression
expression' variant = frequency $
    [ (1, FunctionApp <$> expression' NonLiteral <*> (resize 4 $ listOf1 arbitrary))
    , (3, Identifier <$> rightIdentifier) ]
    ++ case variant of WithLiterals -> [ (3, IntLiteral <$> positiveNum)
                                       , (3, DoubleLiteral <$> positiveNum)
                                       , (3, StringLiteral <$> arbitrary) ]
                       _            -> []

instance Arbitrary Expression where
    arbitrary = expression' WithLiterals

instance Arbitrary Bound where
    arbitrary = frequency
              [ (2, StaticBound <$> (IntLiteral <$> positiveNum))
              , (1, DynamicWithStaticBound <$> arbitrary <*> (IntLiteral <$> positiveNum)) ]

maybeGen :: Gen a -> Gen (Maybe a)
maybeGen g = oneof [Just <$> g, return Nothing]

statements3 :: Gen [Statement]
statements3 = resize 3 $ listOf1 arbitrary

instance Arbitrary Statement where
    arbitrary = frequency
              [ (3, Assignment <$> arbitrary <*> lowerIdentifier <*> arbitrary)
              , (1, If <$> arbitrary <*> statements3 <*> maybeGen statements3)
              , (2, For <$> lowerIdentifier <*> arbitrary <*> arbitrary <*> statements3)
              , (3, Return <$> arbitrary)
              , (3, NakedExpr <$> arbitrary) ]

data TypeLiteralVariant = WithVar | WithUnit | WithNothing

typeLiteral :: TypeLiteralVariant -> Gen TypeLiteral
typeLiteral variant = frequency $
    [ (3, Parameter <$> lowerLetter)
    , (3, Concrete <$> rightCapitalIdentifier)
    , (1, Generic <$> rightCapitalIdentifier <*> (resize 3 $ listOf1 (typeLiteral WithNothing)))
    , (2, Function <$> typeLiteral WithNothing <*> typeLiteral WithUnit) ]
    ++ case variant of WithVar     -> [(3, return Var)]
                       WithUnit    -> [(3, return Unit)]
                       WithNothing -> []

instance Arbitrary TypeLiteral where
    arbitrary = typeLiteral WithVar

import' :: Gen Declaration
import' = oneof [Import <$> rightCapitalIdentifier
                , ImportAs <$> rightCapitalIdentifier <*> capitalIdentifier]

numArgs :: TypeLiteral -> Int
numArgs (Function _ r) = 1 + numArgs r
numArgs _              = 0

function :: Gen (Declaration, Declaration)
function = do
    name <- lowerIdentifier
    typ  <- Function <$> typeLiteral WithNothing <*> typeLiteral WithUnit
    let n = numArgs typ
    args <- vectorOf n lowerIdentifier
    body <- statements3
    return ( FunctionType name typ
           , FunctionDecl name args body )

dataType :: Gen Declaration
dataType = oneof [ data'
                 , record ]

data' :: Gen Declaration
data' = Data <$> capitalIdentifier
             <*> (resize 3 $ listOf lowerLetter) <*> (resize 4 $ listOf1 dataCase)

record :: Gen Declaration
record = Record <$> capitalIdentifier
                <*> (resize 4 $ listOf1 ((,) <$> lowerIdentifier <*> typeLiteral WithNothing))

dataCase :: Gen (String, [TypeLiteral])
dataCase = (,) <$> capitalIdentifier <*> (resize 3 $ listOf $ typeLiteral WithNothing)

newtype Program = Program [Declaration] deriving Show

instance Arbitrary Program where
    arbitrary = do
        imports <- resize 3 $ listOf1 import'
        dataTypes <- resize 3 $ listOf1 dataType
        functions <- concatMap (\(t, d) -> [t, d]) <$> (resize 4 $ listOf1 function)
        return $ Program $ imports ++ dataTypes ++ functions

test :: IO ()
test = do
    progs <- sample' (arbitrary :: Gen Program)
    let str = intercalate "\n\n" $ map (prettyShow . (\(Program d) -> d)) progs
    writeFile "randomPrograms" str
