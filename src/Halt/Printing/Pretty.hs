{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Halt.Printing.Pretty where

import Data.List
import Data.Char
import Halt.AST

class PrettyShow a where
    prettyShow :: a -> String

unlines' :: [String] -> String --Avoids the newline at the end
unlines' = intercalate "\n"

inParentheses :: String -> String
inParentheses s = '(' : (s ++ ")")

indent :: [String] -> String
indent =  unlines' . map ("    " ++) . lines . unlines'

showStatements :: [Statement] -> String
showStatements = indent . map prettyShow

parenthesesIf :: Bool -> String -> String
parenthesesIf b = if b then inParentheses else id

parenthesesUnless :: Bool -> String -> String
parenthesesUnless b = if not b then inParentheses else id

isFunctionApp :: Expression -> Bool
isFunctionApp (FunctionApp _ _) = True
isFunctionApp _                 = False

parenthesesIfFunctionApp :: Expression -> String
parenthesesIfFunctionApp e = parenthesesIf (condition e) (prettyShow e)
    where condition (Identifier s) | isOperator s = True
                                   | otherwise    = False
          condition (FunctionApp _ _) = True
          condition _                 = False

isOperator :: String -> Bool
isOperator = all isSymbol

isSimpleType :: TypeLiteral -> Bool
isSimpleType (Generic _ _)  = False
isSimpleType (Function _ _) = False
isSimpleType _              = True

isFunction :: TypeLiteral -> Bool
isFunction (Function _ _) = True
isFunction _              = False

instance PrettyShow Declaration where
    prettyShow = \case
        Import s           -> "import " ++ s
        ImportAs m a       -> "import " ++ m ++ " as " ++ a
        FunctionType n t   -> n ++ " :: " ++ prettyShow t
        FunctionDecl n a b -> n ++ " " ++ unwords a ++ " ->\n" ++ showStatements b
        Data n g c         -> "data " ++ n ++ " " ++ intersperse ' ' g
                           ++ " = " ++ intercalate " | " (map prettyShow c)
        Record n f         -> "record " ++ n ++ " =\n"
                           ++ unlines' (map (("\t" ++) . (\(fn, t) -> fn ++ " :: " ++ prettyShow t)) f)

instance PrettyShow (String, [TypeLiteral]) where
    prettyShow (c, ts) = c ++ " "
                     ++ unwords (map (\t -> parenthesesUnless (isSimpleType t) $ prettyShow t) ts)

instance PrettyShow TypeLiteral where
    prettyShow = \case
        Parameter c  -> return c
        Concrete s   -> s
        Generic s t  -> prettyShow (s, t)
        Function f t -> parenthesesIf (isFunction t) (prettyShow f) ++ " -> " ++ prettyShow t
        Var          -> "var"
        Unit         -> "()"

instance PrettyShow Statement where
    prettyShow = \case
        Assignment t n v -> parenthesesUnless (isSimpleType t) (prettyShow t)
                         ++ " " ++ n ++ " = " ++ prettyShow v
        If c t e         -> "if " ++ prettyShow c ++ " then\n" ++ showStatements t
                         ++ maybe "" (("\nelse\n" ++) . showStatements) e
        For v s bnd bdy  -> "for " ++ v ++ " from " ++ prettyShow s ++ " to " ++ prettyShow bnd
                         ++ "\n" ++ showStatements bdy
        Return v         -> "return " ++ prettyShow v
        NakedExpr e      -> prettyShow e

instance PrettyShow Bound where
    prettyShow (StaticBound e) = prettyShow e
    prettyShow (DynamicWithStaticBound d s) = prettyShow d ++ " | " ++ prettyShow s

instance PrettyShow Expression where
    prettyShow = \case
        FunctionApp f a -> parenthesesIfFunctionApp f ++ " "
                        ++ unwords (map parenthesesIfFunctionApp a)
        IntLiteral i    -> show i
        DoubleLiteral d -> show d
        StringLiteral s -> show s
        Identifier s    -> s

instance PrettyShow [Declaration] where
    prettyShow d = unlines $ map prettyShow d

{-

testPrint = do
    basic <- basicAST
    putStrLn $ prettyShow basic

testParse = do
    basic <- basicAST
    let str = prettyShow basic
    putStrLn "Original:"
    print basic
    putStrLn "New:"
    let new = parseHelper program str
    print new
    putStrLn $ "Equal: " ++ show (basic == new)
-}
