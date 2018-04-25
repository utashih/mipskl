{-# LANGUAGE RecursiveDo #-}
module Assembler where 

import Control.Monad (forM, forM_, liftM2, mapM)
import Control.Monad.Tardis --(Tardis, runTardis)
import Data.Composition ((.:), (.:.))
import Data.List (genericLength)
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Instruction (Instruction(..), rIns, iIns, jIns, 
                    mnemonicToOpcode, mnemonicToFunct, bytecode)
import Parser (ASTExpr(..), ASTInstruction(..), ASTStatement(..), parseASM)
import Text.Printf (printf)
import Util (partialSum, wordHex)
import qualified Data.Map.Strict as M 

type Address = Integer
type SymbolTable = M.Map String Address
type Assembler a = Tardis SymbolTable SymbolTable a

extractInstruction :: ASTStatement -> Maybe ASTInstruction
extractInstruction stmt = case stmt of 
    ASInstn ins -> Just ins
    ASLabel _   -> Nothing

extractLabel :: ASTStatement -> Maybe String 
extractLabel stmt = case stmt of 
    ASLabel label -> Just label
    ASInstn _     -> Nothing


assemble :: [ASTStatement] -> Either String [Instruction]
assemble stmts = assembled
    where 
        assembled :: Either String [Instruction]
        assembled = instructions where
            (instructions, _) = runTardis (assembleInstructions 0 stmts) (M.empty, M.empty)
        
        assembleInstructions :: Address -> [ASTStatement] -> Assembler (Either String [Instruction]) 
        assembleInstructions _ [] = return (Right [])
        assembleInstructions addr (stmt : stmts) = case stmt of 
            ASLabel label -> do 
                modifyBackwards (M.insert label addr)
                modifyForwards  (M.insert label addr)
                assembleInstructions addr stmts
            ASInstn ins -> do
                let these = case ins of 
                        AITen (AESym mnemonic) (AEReg rd) (AEReg rs) (AEReg rt) -> 
                            let opcode = mnemonicToOpcode mnemonic
                                funct = mnemonicToFunct mnemonic
                            in case mnemonic of 
                                "add"   -> [rIns opcode rs rt rd 0 funct]
                                "sub"   -> [rIns opcode rs rt rd 0 funct]
                        _ -> [Left "Error"]
                rest <- assembleInstructions (addr + genericLength these) stmts
                return $ liftM2 (++) (sequence these) rest

            

