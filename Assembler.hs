module Assembler (assemble) where 

import Control.Applicative ((<|>))
import Control.Monad.Tardis (Tardis, getFuture, getPast, 
                             modifyBackwards, modifyForwards, runTardis)
import Data.List (genericLength)
import Instruction (Instruction(..), rIns, iIns, jIns, 
                    mnemonicToOpcode, mnemonicToFunct)
import Parser (ASTExpr(..), ASTInstruction(..), ASTStatement(..))
import qualified Data.Map.Strict as M 

type Address = Integer
type SymbolTable = M.Map String Address
type Assembler a = Tardis SymbolTable SymbolTable a


assemble :: [ASTStatement] -> Either String [Instruction]
assemble stmts = assembled where
    assembled :: Either String [Instruction]
    assembled = concat <$> instructions
        where (instructions, _) = runTardis (assembleInstructions 0 stmts) (M.empty, M.empty)

assembleInstructions :: Address -> [ASTStatement] -> Assembler (Either String [[Instruction]])
assembleInstructions _ [] = return (Right [])
assembleInstructions addr (stmt : stmts) = case stmt of 
    ASLabel label -> do
        modifyBackwards (M.insert label addr)
        modifyForwards (M.insert label addr)
        assembleInstructions addr stmts
    ASInstn ins -> do
        bytecodes <- case ins of 
            AITen (AESym mnemonic) (AEReg rd) (AEReg rs) (AEReg rt) -> 
                let opcode = mnemonicToOpcode mnemonic
                    funct = mnemonicToFunct mnemonic
                in case mnemonic of 
                    "add"   -> return [rIns opcode rs rt rd 0 funct]
                    "sub"   -> return [rIns opcode rs rt rd 0 funct]
                    "and"   -> return [rIns opcode rs rt rd 0 funct]
                    "or"    -> return [rIns opcode rs rt rd 0 funct]
                    "slt"   -> return [rIns opcode rs rt rd 0 funct]
                    _       -> return [Left $ "Unsupported tenary mnemonic: <" ++ mnemonic ++ ">"]
            AITen (AESym mnemonic) (AEReg rt) (AEReg rs) (AEImm immed) ->
                let opcode = mnemonicToOpcode mnemonic
                    funct = mnemonicToFunct mnemonic
                in case mnemonic of 
                    "addi"  -> return [iIns opcode rs rt immed]
                    "ori"   -> return [iIns opcode rs rt immed]
                    "sll"   -> return [rIns opcode "$zero" rs rt immed funct]
                    "srl"   -> return [rIns opcode "$zero" rs rt immed funct]
                    "slti"  -> return [iIns opcode rs rt immed]
                    _       -> return [Left $ "Unsupported tenary mnemonic: <" ++ mnemonic ++ ">"]
            AITen (AESym mnemonic) (AEReg rs) (AEReg rt) (AESym label) ->
                let opcode = mnemonicToOpcode mnemonic
                in case mnemonic of  
                    "beq"   -> do
                        bw <- getFuture 
                        fw <- getPast 
                        let inst = case M.lookup label bw <|> M.lookup label fw of 
                                Just target -> iIns opcode rs rt (target - addr - 1)
                                Nothing -> Left $ "Undefined label: '" ++ label ++ "'"
                        return [inst]
                    "bne"   -> do
                        bw <- getFuture 
                        fw <- getPast 
                        let inst = case M.lookup label bw <|> M.lookup label fw of 
                                Just target -> iIns opcode rs rt (target - addr - 1)
                                Nothing -> Left $ "Undefined label: '" ++ label ++ "'"
                        return [inst]
                    _       -> return [Left $ "Unsupported tenary mnemonic: <" ++ mnemonic ++ ">"]
            AIOff (AESym mnemonic) (AEReg rt) (AEImm immed) (AEReg rs) ->
                let opcode = mnemonicToOpcode mnemonic
                in case mnemonic of 
                    "lw"    -> return [iIns opcode rs rt immed]
                    "sw"    -> return [iIns opcode rs rt immed]
                    _       -> return [Left $ "Unsupported mnemonic with offset: <" ++ mnemonic ++ ">"]
            AIBin (AESym mnemonic) (AEReg rt) (AEImm immed) ->
                let opcode = mnemonicToOpcode mnemonic
                in case mnemonic of 
                    "lui"   -> return [iIns opcode "$zero" rt immed]
                    _       -> return [Left $ "Unsupported binary mnemonic: <" ++ mnemonic ++ ">"]
            AIJmp (AESym mnemonic) (AESym label) ->
                let opcode = mnemonicToOpcode mnemonic
                in case mnemonic of 
                    "j"     -> do
                        bw <- getFuture 
                        fw <- getPast 
                        let inst = case M.lookup label bw <|> M.lookup label fw of 
                                Just target -> jIns opcode target
                                Nothing -> Left $ "Undefined label: '" ++ label ++ "'"
                        return [inst]
                    "jal"   -> do
                        bw <- getFuture 
                        fw <- getPast 
                        let inst = case M.lookup label bw <|> M.lookup label fw of 
                                Just target -> jIns opcode target
                                Nothing -> Left $ "Undefined label: '" ++ label ++ "'"
                        return [inst]
                    _       -> return [Left $ "Unsupported unary mnemonic: <" ++ mnemonic ++ ">"]
            AIJmp (AESym mnemonic) (AEReg rs) ->
                let opcode = mnemonicToOpcode mnemonic
                    funct = mnemonicToFunct mnemonic
                in case mnemonic of 
                    "jr"    -> return [rIns opcode rs "$zero" "$zero" 0 funct]
                    _       -> return [Left $ "Unsupported unary mnemonic: <" ++ mnemonic ++ ">"]
            _ -> return [Left "Ill-formatted instruction"]
        restInstructions <- assembleInstructions (addr + genericLength bytecodes) stmts 
        return $ do 
            x <- sequence bytecodes
            xs <- restInstructions 
            return (x:xs)
        

