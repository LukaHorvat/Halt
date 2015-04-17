module Main where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.Runner
import Halt.Parsing.Parse
import Halt.Parsing.Indent
import Halt.AST
import Halt.Printing.Pretty
import Parsing.AST

conf :: Config
conf = defaultConfig
     { configColorMode = ColorAuto }

true :: Expression
true = Identifier "True"

withParser :: (Show a, Eq a) => Parser a -> (String -> a -> Expectation)
withParser p from to = parseHelper p from `shouldBe` to

main :: IO ()
main = hspecWith conf $ do
    describe "declaration parser" $ do
        let parsesTo = withParser declaration
        it "parses imports" $ do
            "import Test" `parsesTo` Import "Test"
            "import Test as T" `parsesTo` ImportAs "Test" "T"
        it "parses function type declarations" $
            "fn :: a -> Int -> F a String -> ()" `parsesTo`
                ( FunctionType "fn"
                $ Function (Parameter 'a')
                $ Function (Concrete "Int")
                $ Function (Generic "F" [Parameter 'a', Concrete "String"])
                Unit )
        it "parses function declarations" $
            "fn x -> return True" `parsesTo`
                FunctionDecl "fn" ["x"] [Return true]
        it "parses data declarations" $
            "data List a = Cons a (List a) | Nil" `parsesTo`
                Data "List" ['a']
                     [ ("Cons", [Parameter 'a', Generic "List" [Parameter 'a']])
                     , ("Nil", []) ]
        it "parses record declarations" $
            "record Guy =\n\tname :: String\n\tsay :: () -> ()" `parsesTo`
                Record "Guy"
                       [ ("name", Concrete "String")
                       , ("say", Function Unit Unit) ]

    describe "statement parser" $ do
        let parsesTo = withParser statement
        let ifStatement = If true [NakedExpr true] $ Just [NakedExpr true]
        it "parses assignments" $ do
            "var a = 5" `parsesTo` Assignment Var "a" (IntLiteral 5)
            "(a -> b) a = 1" `parsesTo`
                Assignment (Function (Parameter 'a') (Parameter 'b'))
                           "a" (IntLiteral 1)
        it "parses if statements" $ do
            "if True then True else True" `parsesTo` ifStatement
            "if True then\n\tTrue\nelse True" `parsesTo` ifStatement
            "if True then True\nelse True" `parsesTo` ifStatement
            "if True then True else\n\tTrue" `parsesTo` ifStatement
            "if True then\n\tTrue\nelse\n\tTrue" `parsesTo` ifStatement

        it "parses for loops" $ do
            "for i from 0 to x | 5\n\treturn True" `parsesTo`
                For "i" (IntLiteral 0)
                    (DynamicWithStaticBound (Identifier "x") (IntLiteral 5))
                    [ Return true ]
            "for i from 0 to 5\n\treturn True" `parsesTo`
                For "i" (IntLiteral 0) (StaticBound (IntLiteral 5)) [Return true]
        it "parses return statements" $ do
            "return True" `parsesTo` Return true
        it "parses naked expressions" $ do
            "True" `parsesTo` NakedExpr true

    describe "expression parser" $ do
        let parsesTo = withParser expression
        it "parses int literals" $ do
            "5" `parsesTo` IntLiteral 5
        it "parses double literals" $ do
            "5.0" `parsesTo` DoubleLiteral 5
        it "parses string literals" $ do
            "\"testtest\\n\\\"\"" `parsesTo` StringLiteral "testtest\n\""
        it "parses identifiers" $ do
            "lowerCase" `parsesTo` Identifier "lowerCase"
            "CapitalCase" `parsesTo` Identifier "CapitalCase"
            "Module.Then.lowerCase" `parsesTo` Identifier "Module.Then.lowerCase"
            "Module.Then.CapitalCase" `parsesTo` Identifier "Module.Then.CapitalCase"
            "fancy_characters'" `parsesTo` Identifier "fancy_characters'"
        it "parses function application" $ do
            "fn y 5 True" `parsesTo`
                FunctionApp (Identifier "fn") [Identifier "y", IntLiteral 5, true]
            "(map fn) list" `parsesTo`
                FunctionApp (FunctionApp (Identifier "map") [Identifier "fn"]) [Identifier "list"]
        it "parses infix expressions" $ do
            "1 * 2 + 3 * 4" `parsesTo`
                FunctionApp (Identifier "+")
                            [ FunctionApp (Identifier "*") [IntLiteral 1, IntLiteral 2]
                            , FunctionApp (Identifier "*") [IntLiteral 3, IntLiteral 4] ]
    describe "program parser" $ do
        it "is inverse to pretty printing" $ property $
            \(Program decls) -> (parseHelper program . prettyShow) decls == decls
