module Assembler where 

import Parser (ASTExpr(..), ASTInstruction(..), ASTLine(..), parseASM)
import Register (Register(..), register)

data Instruction
    = RIns Opcode Register Register Register Shamt Funct
    | IIns Opcode Register Register Integer
    | JIns Opcode Integer
    deriving (Show, Eq)

type Opcode = Integer 
type Shamt = Integer
type Funct = Integer

rIns :: Integer -> String -> String -> String -> Integer -> Integer -> Either String Instruction
rIns opcode _ _ _ _ _ | opcode < 0|| opcode >= 2^6 = Left $ show opcode ++ " cannot be opcode"
rIns _ _ _ _ shamt _ | shamt < 0 || shamt >= 2^5 = Left $ show shamt ++ " cannot be shamt"
rIns _ _ _ _ _ funct | funct < 0 || funct >= 2^6 = Left $ show funct ++ " cannot be funct"
rIns opcode srs srt srd shamt funct = 
    let mregs = do  
        rs <- register srs 
        rt <- register srt
        rd <- register srd 
        return (rs, rt, rd)
    in case mregs of 
        Nothing           -> Left "Wrong register"
        Just (rs, rt, rd) -> Right $ RIns opcode rs rt rd shamt funct 

iIns :: Integer -> String -> String ->Integer -> Either String Instruction
iIns opcode _ _ _ | opcode < 0 || opcode >= 2^6 = Left $ show opcode ++ " cannot be opcode"
iIns _ _ _ immed | immed < -2^15 || immed >= 2^16 = Left $ show immed ++ " cannot be immediate"
iIns opcode srs srt immed = 
    let mregs = do 
        rs <- register srs 
        rt <- register srt 
        return (rs, rt)
    in case mregs of 
        Nothing       -> Left "Wrong register"
        Just (rs, rt) -> Right $ IIns opcode rs rt immed

jIns :: Integer -> Integer -> Either String Instruction 
jIns opcode _ | opcode < 0 || opcode >= 2^6 = Left $ show opcode ++ " cannot be opcode"
jIns opcode addr = Right $ JIns opcode addr

mnemonicToOpcode :: String -> Integer 
mnemonicToOpcode mnemonic = case mnemonic of 
    "add"   -> 0x00
    "addi"  -> 0x08
    "addiu" -> 0x09
    "addu"  -> 0x00
    "and"   -> 0x00
    "andi"  -> 0x0c
    "beq"   -> 0x04
    "bne"   -> 0x05
    "j"     -> 0x02
    "jal"   -> 0x03
    "jr"    -> 0x00
    "lbu"   -> 0x24
    "lhu"   -> 0x25
    "ll"    -> 0x30
    "lui"   -> 0x0f
    "lw"    -> 0x23
    "nor"   -> 0x00
    "or"    -> 0x00
    "ori"   -> 0x0d
    "slt"   -> 0x00
    "slti"  -> 0x0a
    "sltiu" -> 0x0b
    "sltu"  -> 0x00
    "sll"   -> 0x00
    "srl"   -> 0x00
    "sb"    -> 0x28
    "sc"    -> 0x38
    "sh"    -> 0x29
    "sw"    -> 0x2b
    "sub"   -> 0x00
    "subu"  -> 0x00
    _       -> -1
    
mnemonicToFunct :: String -> Integer 
mnemonicToFunct mnemonic = case mnemonic of 
    "add"   -> 0x20 
    "addu"  -> 0x21
    "and"   -> 0x24
    "jr"    -> 0x08
    "nor"   -> 0x27
    "or"    -> 0x25
    "slt"   -> 0x2a
    "sltu"  -> 0x2b
    "sll"   -> 0x00
    "srl"   -> 0x02
    "sub"   -> 0x22
    "subu"  -> 0x23
    _       -> -1

encode :: ASTInstruction -> Either String Instruction
encode (AITen (AESym mnemonic) (AESym rs) (AESym rt) (AESym rd))
    = rIns (mnemonicToOpcode mnemonic) rs rt rd 0 0

