{-# LANGUAGE FlexibleContexts #-}
module Halt.Parsing.Elements where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding (token, State)
import Halt.Parsing.Common
import Halt.Utility
import Control.Monad
import Data.Char
import Data.Monoid
import Data.Maybe

whiteSpace :: CharStream s m => ParsecT s u m ()
whiteSpace = skipMany (skipMany1 (oneOf " \t") <|> comment)

comment :: CharStream s m => ParsecT s u m ()
comment = delimited commentBody <|> (try (word "--") *> skipMany (noneOf "\n")) <?> "comment"
    where delimited = between (word "{-") (word "-}")
          commentBody = skipMany (delimited commentBody <|> nonComment)
          nonComment  = do
              mb <- optionMaybe (lookAhead (word "-}"))
              case mb of Nothing -> void anyChar
                         Just _  -> fail "unexpected end of comment"

word :: CharStream s m => String -> ParsecT s u m String
word str = string str <* whiteSpace

identifierChar :: CharStream s m => ParsecT s u m Char
identifierChar = letter <|> oneOf "_'" <|> digit

lowerIdentifier :: CharStream s m => ParsecT s u m String
lowerIdentifier = lower <:> option "" (many identifierChar) <* whiteSpace <?> "lower case identifier"

capitalIdentifier :: CharStream s m => ParsecT s u m String
capitalIdentifier = upper <:> option "" (many identifierChar) <* whiteSpace <?> "upper case identifier"

parens :: CharStream s m => ParsecT s u m a -> ParsecT s u m a
parens p = between (word "(") (word ")") p

intLiteral :: CharStream s m => ParsecT s u m Integer
intLiteral = read <$> (many1 digit <* whiteSpace) <?> "int literal"

doubleLiteral :: CharStream s m => ParsecT s u m Double
doubleLiteral = read <$> (many digit <* whiteSpace) <++> (char '.' <:> many1 digit) <?> "double literal"

--[taken from Text.Parsec.Token] modified formatting and converted to applicative style
stringLiteral :: CharStream s m => ParsecT s u m String
stringLiteral = (collect <$> str) <?> "string literal"
    where str     = between (char '"') (word "\"" <?> "end of string") (many stringChar)
          collect = fromJust . foldr (<>) (Just "") . map (fmap return)

stringChar :: CharStream s m => ParsecT s u m (Maybe Char)
stringChar = (Just <$> stringLetter)
         <|> stringEscape
         <?> "string character"

stringLetter :: CharStream s m => ParsecT s u m Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape :: CharStream s m => ParsecT s u m (Maybe Char)
stringEscape = char '\\' *> escape
    where escape = (escapeGap *> return Nothing)
               <|> (escapeEmpty *> return Nothing)
               <|> (Just <$> escapeCode)

escapeEmpty :: CharStream s m => ParsecT s u m Char
escapeEmpty = char '&'

escapeGap :: CharStream s m => ParsecT s u m Char
escapeGap = many1 space *> (char '\\' <?> "end of string gap")

escapeCode :: CharStream s m => ParsecT s u m Char
escapeCode = charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

charControl :: CharStream s m => ParsecT s u m Char
charControl = char '^' *> (toEnum <$> (subtract $ fromEnum 'A') <$> fromEnum <$> upper)

charNum :: CharStream s m => ParsecT s u m Char
charNum = toEnum <$> fromInteger <$> num
    where num = decimal
            <|> (char 'o' *> number 8  octDigit)
            <|> (char 'x' *> number 16 hexDigit)

charEsc :: CharStream s m => ParsecT s u m Char
charEsc = choice $ map parseEsc escMap
    where parseEsc (c, code) = char c *> return code

charAscii :: CharStream s m => ParsecT s u m Char
charAscii = choice $ map parseAscii asciiMap
    where parseAscii (asc, code) = try $ string asc *> return code

decimal :: CharStream s m => ParsecT s u m Integer
decimal = number 10 digit

hexadecimal :: CharStream s m => ParsecT s u m Integer
hexadecimal = oneOf "xX" *> number 16 hexDigit

octal :: CharStream s m => ParsecT s u m Integer
octal = oneOf "oO" *> number 8 octDigit

number :: CharStream s m => Integer -> ParsecT s u m Char -> ParsecT s u m Integer
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

operator :: CharStream s m => ParsecT s u m String
operator = many (satisfy isSymbol)
