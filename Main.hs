module Main where 

import Parser (parseASM)
import System.Environment (getArgs)

main :: IO ()
main = do 
    putStrLn "mipskl"
    args <- getArgs
    let (filename:_) = args 
    src <- readFile filename
    case parseASM src of 
        Left  err -> fail $ show err
        Right ast -> print ast
    return ()
    