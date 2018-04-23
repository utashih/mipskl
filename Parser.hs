module Parser where 

import Data.Char (isSpace)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Number (decimal, int)
import Text.ParserCombinators.Parsec (Parser)
import Text.ParserCombinators.Parsec.Char (alphaNum, char, letter, satisfy)
import Text.ParserCombinators.Parsec.Combinator (many1, sepBy)
import Text.ParserCombinators.Parsec.Prim ((<|>), many, parse, try)
import Util ((|>))


data ASTExpr = AESym String
             | AEReg String
             | AEImm Integer 
             deriving (Show, Eq)

data ASTInstruction 
    = AITen ASTExpr ASTExpr ASTExpr ASTExpr
    | AIOff ASTExpr ASTExpr ASTExpr ASTExpr
    | AIBin ASTExpr ASTExpr ASTExpr
    | AIJmp ASTExpr ASTExpr
    deriving (Show, Eq)

data ASTLine
    = ALLabel ASTExpr
    | ALInstruction ASTInstruction
    | ALLabelledInstruction ASTExpr ASTInstruction
    deriving (Show, Eq)

removeComment :: String -> String 
removeComment = takeWhile (/= '#')

removeComments :: String -> String
removeComments src = src 
  |> lines 
  |> map removeComment
  |> filter (\x -> dropWhile isSpace x /= "") 
  |> unlines

whitespace :: Parser Char
whitespace = satisfy (\c -> isSpace c && c /= '\n')

whitespaces :: Parser String
whitespaces = many whitespace

symbol :: Parser ASTExpr
symbol = do 
    initial <- char '_' <|> letter
    rest <- many $ char '_' <|> alphaNum
    return $ AESym (initial:rest)

register :: Parser ASTExpr 
register = do 
    _ <- char '$'
    dec <- many1 alphaNum
    return $ AEReg ('$':dec)

immediate :: Parser ASTExpr 
immediate = do 
    imm <- int 
    return $ AEImm imm

expr :: Parser ASTExpr 
expr = symbol <|> register <|> immediate 


char' :: Char -> Parser ()
char' c = whitespaces >> char c >> whitespaces >> return ()

insTenary :: Parser ASTInstruction
insTenary = do 
    op <- expr 
    _  <- whitespaces
    a0 <- expr 
    _  <- char' ','
    a1 <- expr 
    _  <- char' ','
    a2 <- expr 
    return $ AITen op a0 a1 a2

insBinary :: Parser ASTInstruction
insBinary = do 
    op <- expr 
    _  <- whitespaces
    a0 <- expr 
    _  <- char' ','
    a1 <- expr 
    return $ AIBin op a0 a1

insOffset :: Parser ASTInstruction
insOffset = do 
    op  <- expr 
    _   <- whitespaces 
    a0  <- expr
    _   <- char' ','
    imm <- expr
    _   <- char' '(' 
    a1  <- expr 
    _   <- char' ')'
    return $ AIOff op a0 imm a1

insJump :: Parser ASTInstruction
insJump = do 
    op   <- expr
    _    <- whitespaces
    addr <- expr
    return $ AIJmp op addr

instruction :: Parser ASTInstruction
instruction = try insTenary <|> try insOffset <|> try insBinary <|> insJump

lineLabel :: Parser ASTLine
lineLabel = do 
    _ <- whitespaces 
    label <- symbol
    _ <- char' ':' 
    _ <- char '\n'
    return $ ALLabel label

lineInstruction :: Parser ASTLine
lineInstruction = do 
    _ <- whitespaces 
    ins <- instruction 
    _ <- char' '\n' 
    return $ ALInstruction ins 

lineLabelled :: Parser ASTLine
lineLabelled = do 
    _ <- whitespaces 
    label <- symbol 
    _ <- char' ':' 
    ins <- instruction
    _ <- char' '\n' 
    return $ ALLabelledInstruction label ins 

mipsline :: Parser ASTLine
mipsline = try lineLabelled <|> try lineLabel <|> lineInstruction

mipslines :: Parser [ASTLine]
mipslines = many mipsline 

parseASM :: String -> Either String [ASTLine]
parseASM src = case parse mipslines "mipskl" (removeComments src) of 
    Left error -> Left $ show error
    Right lns  -> Right lns
