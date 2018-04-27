{-# LANGUAGE MultiWayIf #-}
module Disassembler where 

import Control.Monad (mapM, mapM_, sequence)
import Data.Bits ((.&.), shiftR) 
import Data.List (stripPrefix)
import Data.Word (Word32)
import Instruction (Instruction(..), functToMnemonic, opcodeToMnemonic, unbytecode)
import OutputCOE (headText)
import Parser (ASTExpr(..), ASTInstruction(..), ASTStatement(..))
import Register (Register(..))
import Text.Printf (printf)
import qualified Numeric (readHex)
import qualified Data.Map.Strict as Map 
import qualified Data.Set as Set  


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

makeAST :: Instruction -> ASTInstruction
makeAST (RIns opcode rs rt rd shamt funct) = 
    if | mnemonic `elem` ["sll", "srl", "sra"] ->
            AITen (AESym mnemonic) (AEReg (show rd)) (AEReg (show rt)) (AEImm shamt)
       | mnemonic == "jr" ->
            AIJmp (AESym mnemonic) (AEReg (show rs))
       | mnemonic == "jalr" ->
            AIBin (AESym mnemonic) (AEReg (show rs)) (AEReg (show rd)) 
       | otherwise -> 
            AITen (AESym mnemonic) (AEReg (show rd)) (AEReg (show rs)) (AEReg (show rt))  
    where Right mnemonic = functToMnemonic funct
makeAST (IIns opcode rs rt immed) = 
    if | mnemonic `elem` ["lb", "lbu", "lh", "lhu", "lw", "sb", "sh", "sw"] -> 
            AIOff (AESym mnemonic) (AEReg (show rt)) (AEImm immed) (AEReg (show rs))
       | mnemonic `elem` ["beq", "bne"] -> 
            AITen (AESym mnemonic) (AEReg (show rs)) (AEReg (show rt)) (AEImm immed)
       | mnemonic == "lui" ->
            AIBin (AESym mnemonic) (AEReg (show rt)) (AEImm immed) 
       | otherwise ->
            AITen (AESym mnemonic) (AEReg (show rt)) (AEReg (show rs)) (AEImm immed)
    where Right mnemonic = opcodeToMnemonic opcode
makeAST (JIns opcode immed) = 
    AIJmp (AESym mnemonic) (AEImm immed)
    where Right mnemonic = opcodeToMnemonic opcode


labelledLines :: [ASTInstruction] -> Either String (Map.Map Integer String)
labelledLines ins = do 
    lines <- Set.toList <$> getLabelledLines 0 ins 
    return $ Map.fromList $ zipWith (\line no -> (line, "l" ++ show no)) lines [1..]
    where 
        getLabelledLines :: Integer -> [ASTInstruction] -> Either String (Set.Set Integer)
        getLabelledLines _ [] = Right Set.empty
        getLabelledLines addr (ins:restIns) = do 
            f <- case ins of 
                AITen (AESym mnemonic) rs@(AEReg _) rt@(AEReg _) (AEImm immed) | mnemonic `elem` ["beq", "bne"] -> 
                    Right $ Set.insert (addr + 1 + immed)
                AIJmp (AESym mnemonic) (AEImm immed) ->
                    Right $ Set.insert immed 
                _ -> Right id 
            rest <- getLabelledLines (addr + 1) restIns
            return $ f rest 
            

addLabels :: [ASTInstruction] -> Either String [ASTStatement]
addLabels ins = do 
    mapping <- labelledLines ins 
    stmts <- modifyLabels mapping 0 ins
    return $ concat stmts  
    where 
        modifyLabels :: Map.Map Integer String -> Integer -> [ASTInstruction] -> Either String [[ASTStatement]]
        modifyLabels _ _ [] = Right []
        modifyLabels mapping addr (ins:restIns) = do
            this <- case ins of 
                AITen (AESym mnemonic) rs@(AEReg _) rt@(AEReg _) (AEImm immed) | mnemonic `elem` ["beq", "bne"] -> 
                    case Map.lookup (addr + 1 + immed) mapping of 
                        Just label -> Right $ AITen (AESym mnemonic) rs rt (AESym label)
                        Nothing    -> Left "Huh?"
                AIJmp (AESym mnemonic) (AEImm immed) ->
                    case Map.lookup immed mapping of 
                        Just label -> Right $ AIJmp (AESym mnemonic) (AESym label)
                        Nothing    -> Left "Huh?"
                _ -> Right ins 
            rest <- modifyLabels mapping (addr + 1) restIns 
            case Map.lookup addr mapping of 
                Just label -> return ([ASLabel label, ASInstn this] : rest)
                Nothing    -> return ([ASInstn this] : rest)
                

prettyPrint :: ASTStatement -> String 
prettyPrint (ASLabel label) = label ++ ": "
prettyPrint (ASInstn ins) = case ins of 
    AITen (AESym mnemonic) (AEReg rd) (AEReg rs) (AEReg rt) -> 
        printf "    %-8s%s, %s, %s" mnemonic rd rs rt 
    AITen (AESym mnemonic) (AEReg rt) (AEReg rs) (AEImm immed) -> 
        printf "    %-8s%s, %s, %d" mnemonic rt rs immed
    AITen (AESym mnemonic) (AEReg rs) (AEReg rt) (AESym label) ->
        printf "    %-8s%s, %s, %s" mnemonic rs rt label  
    AIOff (AESym mnemonic) (AEReg rt) (AEImm immed) (AEReg rs) ->
        printf "    %-8s%s, %d(%s)" mnemonic rt immed rs 
    AIBin (AESym mnemonic) (AEReg rt) (AEImm immed) ->
        printf "    %-8s%s, %d" mnemonic rt immed 
    AIBin (AESym mnemonic) (AEReg rs) (AEReg rd) ->
        printf "    %-8s%s, %s" mnemonic rs rd 
    AIJmp (AESym mnemonic) (AESym label) ->
        printf "    %-8s%s" mnemonic label 
    AIJmp (AESym mnemonic) (AEReg rs) -> 
        printf "    %-8s%s" mnemonic rs
    _ -> "Syntax error"


disasm :: String -> Either String [String]
disasm src = do 
    words <- parseFile src 
    ins   <- disassemble words 
    let ast = makeAST <$> ins 
    stmts <- addLabels ast 
    return $ map prettyPrint stmts
