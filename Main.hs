module Main where 

import Assembler (assemble)
import Disassembler (disasm)
import Instruction (bytecode)
import OutputCOE (composeCoe)
import Parser (parseASM)
import System.Environment (getArgs)

asm :: String -> String -> IO () 
asm inputFile outputFile = do 
    src <- readFile inputFile
    case parseASM src >>= assemble of 
        Left  err -> putStrLn err 
        Right ins -> do 
            let bytecodes = map bytecode ins 
            writeFile outputFile $ composeCoe bytecodes 

dasm :: String -> String -> IO () 
dasm inputFile outputFile = do 
    src <- readFile inputFile
    case disasm src of 
        Left  err -> putStrLn err
        Right ins -> writeFile "test.d.asm" (unlines ins)

main :: IO ()
main = do 
    args <- getArgs
    case args of 
        [cmd, inputFile, outputFile] ->  case cmd of 
            "-a" -> asm inputFile outputFile
            "-d" -> dasm inputFile outputFile
            _    -> putStrLn $ "Error: unknown command '" ++ cmd ++ "'"
        _ -> putStrLn "Usage: -<a|d> <input> <output>"

    