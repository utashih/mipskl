{-# LANGUAGE MultiWayIf #-}
module Disassembler where 

import Control.Monad (mapM, mapM_, sequence)
import Data.Bits ((.&.), shiftR) 
import Data.List (stripPrefix)
import Data.Word (Word32)
import Instruction (Instruction(..), functToMnemonic, opcodeToMnemonic, unbytecode)
import OutputCOE (headText)
import Register (Register(..))
import Text.Printf (printf)
import qualified Numeric (readHex)

readHex :: (Eq a, Num a) => String -> Either String a 
readHex str = case Numeric.readHex str of 
    [(result, _)] -> Right result 
    _             -> Left "Parse error: not a hexadecimal textfile"

parseFile :: String -> Either String [Word32]
parseFile src = 
    case stripPrefix headText src of 
        Nothing -> Left "Parse error: ill-formated COE file"
        Just raw -> do 
            let bytecodes = readHex . take 8 <$> lines raw
            sequence bytecodes 


disassemble :: [Word32] -> Either String [Instruction]
disassemble = mapM unbytecode 

prettyPrint :: Instruction -> String
prettyPrint (RIns opcode rs rt rd shamt funct) = 
    if | mnemonic `elem` ["sll", "srl"] ->
            printf "    %-8s%s, %s, %d" mnemonic (show rd) (show rt) shamt
       | mnemonic == "jr" ->
            printf "    %-8s%s" mnemonic (show rs)
       | otherwise -> 
            printf "    %-8s%s, %s, %s" mnemonic (show rd) (show rs) (show rt)
    where Right mnemonic = functToMnemonic funct
prettyPrint (IIns opcode rs rt immed) = 
    if | mnemonic `elem` ["lw", "sw"] -> 
            printf "    %-8s%s, %d(%s)" mnemonic (show rt) immed (show rs) 
       | mnemonic `elem` ["beq", "bne"] -> 
            printf "    %-8s%s, %s, %d" mnemonic (show rs) (show rt) immed 
       | mnemonic == "lui" ->
            printf "    %-8s%s, %d" mnemonic (show rt) immed 
       | otherwise ->
            printf "    %-8s%s, %s, %d" mnemonic (show rt) (show rs) immed 
    where Right mnemonic = opcodeToMnemonic opcode
prettyPrint (JIns opcode immed) = 
    printf "    %-8s%d" mnemonic immed 
    where Right mnemonic = opcodeToMnemonic opcode

test :: IO ()
test = do 
    src <- readFile "test.coe"
    case parseFile src >>= disassemble of 
        Left  err -> putStrLn err
        Right ins -> mapM_ (putStrLn . prettyPrint) ins
    return ()
    