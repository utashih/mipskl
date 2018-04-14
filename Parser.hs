module Parser where 

import Data.Char (isSpace)
import Text.Parsec.Number (decimal)
import Text.ParserCombinators.Parsec (Parser)
import Text.ParserCombinators.Parsec.Char (alphaNum, char, letter, satisfy)
import Text.ParserCombinators.Parsec.Prim ((<|>), many, parse)
import Util ((|>))


data Expr = ESym String
          | EReg Integer
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

label :: Parser Expr
label = do 
    initial <- char '_' <|> letter
    rest <- many $ char '_' <|> alphaNum
    return $ ESym (initial:rest)

register :: Parser Expr 
register = do 
    char '$'
    dec <- decimal
    return $ EReg dec 



