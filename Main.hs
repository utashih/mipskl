module Main where 

import Assembler (assemble)
import Instruction (bytecode)
import OutputCOE (composeCoe)
import Parser (parseASM)
import System.Environment (getArgs)

main :: IO ()
main = do 
    args <- getArgs
    let (inputFile:outputFile:_) = args 
    src <- readFile inputFile
    case parseASM src >>= assemble of 
        Left  err -> putStrLn err
        Right ast -> do 
            let bytecodes = map bytecode ast
            writeFile outputFile $ composeCoe bytecodes
    return ()
    