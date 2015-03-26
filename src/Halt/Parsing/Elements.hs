{-# LANGUAGE FlexibleContexts #-}
module Halt.Parsing.Elements where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding (token, State)
import Halt.Parsing.Common
import Control.Monad
import Data.Char
import Data.Monoid
import Data.Maybe

whiteSpace :: CharStream s => Parsec s u ()
whiteSpace = skipMany (skipMany (oneOf " \t") <|> comment)

comment :: CharStream s => Parsec s u ()
comment = delimited commentBody <|> (try (word "--") *> skipMany (noneOf "\n")) <?> "comment"
    where delimited = between (word "{-") (word "-}")
          commentBody = skipMany (delimited commentBody <|> nonComment)
          nonComment  = do
              mb <- optionMaybe (lookAhead (word "-}"))
              case mb of Nothing -> void anyChar
                         Just _  -> fail "unexpected end of comment"

word :: CharStream s => String -> Parsec s u String
word str = string str <* spaces

identifierChar :: CharStream s => Parsec s u Char
identifierChar = letter <|> oneOf "_'" <|> digit

lowerIdentifier :: CharStream s => Parsec s u String
lowerIdentifier = lower <:> option "" (many identifierChar) <* spaces <?> "lower case identifier"

capitalIdentifier :: CharStream s => Parsec s u String
capitalIdentifier = upper <:> option "" lowerIdentifier <* spaces <?> "upper case identifier"

parens :: CharStream s => Parsec s u String -> Parsec s u String
parens p = between (word "(") (word ")") p

intLiteral :: CharStream s => Parsec s u Integer
intLiteral = read <$> many1 digit

doubleLiteral :: CharStream s => Parsec s u Double
doubleLiteral = read <$> many digit <++> option "" (char '.' <:> many1 digit)

--[taken from Text.Parsec.Token] modified formatting and converted to applicative style
stringLiteral :: CharStream s => Parsec s u String
stringLiteral = (collect <$> str) <?> "literal string"
    where str     = between (char '"') (char '"' <?> "end of string") (many stringChar)
          collect = fromJust . foldr (<>) (Just "") . map (fmap return)

stringChar :: CharStream s => Parsec s u (Maybe Char)
stringChar = (Just <$> stringLetter)
         <|> stringEscape
         <?> "string character"

stringLetter :: CharStream s => Parsec s u Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape :: CharStream s => Parsec s u (Maybe Char)
stringEscape = char '\\' *> escape
    where escape = (escapeGap *> return Nothing)
               <|> (escapeEmpty *> return Nothing)
               <|> (Just <$> escapeCode)

escapeEmpty :: CharStream s => Parsec s u Char
escapeEmpty = char '&'

escapeGap :: CharStream s => Parsec s u Char
escapeGap = many1 space *> (char '\\' <?> "end of string gap")

escapeCode :: CharStream s => Parsec s u Char
escapeCode = charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

charControl :: CharStream s => Parsec s u Char
charControl = char '^' *> (toEnum <$> (subtract $ fromEnum 'A') <$> fromEnum <$> upper)

charNum :: CharStream s => Parsec s u Char
charNum = toEnum <$> fromInteger <$> num
    where num = decimal
            <|> (char 'o' *> number 8  octDigit)
            <|> (char 'x' *> number 16 hexDigit)

charEsc :: CharStream s => Parsec s u Char
charEsc = choice $ map parseEsc escMap
    where parseEsc (c, code) = char c *> return code

charAscii :: CharStream s => Parsec s u Char
charAscii = choice $ map parseAscii asciiMap
    where parseAscii (asc, code) = try $ string asc *> return code

decimal :: CharStream s => Parsec s u Integer
decimal = number 10 digit

hexadecimal :: CharStream s => Parsec s u Integer
hexadecimal = oneOf "xX" *> number 16 hexDigit

octal :: CharStream s => Parsec s u Integer
octal = oneOf "oO" *> number 8 octDigit

number :: CharStream s => Integer -> Parsec s u Char -> Parsec s u Integer
number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
    seq n (return n)

escMap :: [(Char, Char)]
escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

asciiMap :: [([Char], Char)]
asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes :: [String]
ascii2codes = [ "BS", "HT", "LF", "VT", "FF", "CR", "SO"
              , "SI", "EM", "FS", "GS", "RS", "US", "SP" ]

ascii3codes :: [String]
ascii3codes = [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK"
              , "BEL", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK"
              , "SYN", "ETB", "CAN", "SUB", "ESC", "DEL" ]

ascii2 :: [Char]
ascii2 = [ '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO'
         , '\SI', '\EM', '\FS', '\GS', '\RS', '\US', '\SP' ]

ascii3 :: [Char]
ascii3 = [ '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK'
         , '\BEL', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK'
         , '\SYN', '\ETB', '\CAN', '\SUB', '\ESC', '\DEL' ]
--[/taken from Text.Parsec.Token]
