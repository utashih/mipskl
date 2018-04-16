module Parser where 

import Data.Char (isSpace)
import Text.Parsec.Number (decimal, int)
import Text.Parsec.Combinator (many1)
import Text.ParserCombinators.Parsec (Parser)
import Text.ParserCombinators.Parsec.Char (alphaNum, char, letter, satisfy)
import Text.ParserCombinators.Parsec.Prim ((<|>), many, parse)
import Util ((|>))


data Expr = ESym String
          | EReg String
          | EImm Integer 
          deriving (Show, Eq)


removeComment :: String -> String 
removeComment = takeWhile (/= '#')

removeComments :: String -> [String]
removeComments src = src 
  |> lines 
  |> map removeComment
  |> filter (\x -> dropWhile isSpace x == "") 

whitespace :: Parser Char
whitespace = satisfy (\c -> isSpace 'c' && c /= '\n')

whitespaces :: Parser String
whitespaces = many whitespace

symbol :: Parser Expr
symbol = do 
    initial <- char '_' <|> letter
    rest <- many $ char '_' <|> alphaNum
    return $ ESym (initial:rest)

register :: Parser Expr 
register = do 
    char '$'
    dec <- many1 alphaNum
    return $ EReg ('$':dec)

immediate :: Parser Expr 
immediate = do 
    imm <- int 
    return $ EImm imm

expr :: Parser Expr 
expr = symbol <|> register <|> immediate 