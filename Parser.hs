module Parser where 

import Data.Char (isSpace)
import Text.Parsec.Number (decimal, int)
import Text.ParserCombinators.Parsec (Parser)
import Text.ParserCombinators.Parsec.Char (alphaNum, char, letter, satisfy)
import Text.ParserCombinators.Parsec.Combinator (many1, sepBy)
import Text.ParserCombinators.Parsec.Prim ((<|>), many, parse)
import Util ((|>))


data ASTExpr = AESym String
             | AEReg String
             | AEImm Integer 
             deriving (Show, Eq)

data ASTInstruction 
    = AIRtype ASTExpr ASTExpr ASTExpr ASTExpr
    | AIItype ASTExpr ASTExpr ASTExpr ASTExpr
    | AIJtype ASTExpr ASTExpr
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

symbol :: Parser ASTExpr
symbol = do 
    initial <- char '_' <|> letter
    rest <- many $ char '_' <|> alphaNum
    return $ AESym (initial:rest)

register :: Parser ASTExpr 
register = do 
    char '$'
    dec <- many1 alphaNum
    return $ AEReg ('$':dec)

immediate :: Parser ASTExpr 
immediate = do 
    imm <- int 
    return $ AEImm imm

expr :: Parser ASTExpr 
expr = symbol <|> register <|> immediate 

