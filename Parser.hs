module Parser where 

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Number (decimal, int)
import Text.ParserCombinators.Parsec (Parser)
import Text.ParserCombinators.Parsec.Char (alphaNum, char, letter, satisfy)
import Text.ParserCombinators.Parsec.Combinator (many1, optionMaybe, sepBy)
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

data ASTStatement
    = ASLabel String
    | ASInstn ASTInstruction
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
    imm <- optionMaybe expr
    _   <- char' '(' 
    a1  <- expr 
    _   <- char' ')'
    return $ AIOff op a0 (fromMaybe (AEImm 0) imm) a1 

insJump :: Parser ASTInstruction
insJump = do 
    op   <- expr
    _    <- whitespaces
    addr <- expr
    return $ AIJmp op addr

instruction :: Parser ASTInstruction
instruction = try insTenary <|> try insOffset <|> try insBinary <|> insJump

lineLabel :: Parser [ASTStatement]
lineLabel = do 
    _ <- whitespaces 
    (AESym label) <- symbol
    _ <- char' ':' 
    _ <- char '\n'
    return [ASLabel label]

lineInstruction :: Parser [ASTStatement]
lineInstruction = do 
    _ <- whitespaces 
    ins <- instruction 
    _ <- char' '\n' 
    return [ASInstn ins]

lineLabelled :: Parser [ASTStatement]
lineLabelled = do 
    _ <- whitespaces 
    (AESym label) <- symbol 
    _ <- char' ':' 
    ins <- instruction
    _ <- char' '\n' 
    return [ASLabel label, ASInstn ins]

mipsline :: Parser [ASTStatement]
mipsline = try lineLabelled <|> try lineLabel <|> lineInstruction

mipslines :: Parser [ASTStatement]
mipslines = concat <$> many mipsline 

parseASM :: String -> Either String [ASTStatement]
parseASM src = case parse mipslines "mipskl" (removeComments src) of 
    Left error -> Left $ show error
    Right lns  -> Right lns
