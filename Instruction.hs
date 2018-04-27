module Instruction where

import Register (Register(..), register)
import Data.Bits ((.&.), (.|.), shiftL, shiftR) 
import Data.Word (Word32)
import Util (immSext)

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
        Just (rs, rt) -> Right $ IIns opcode rs rt (immed .&. 0xFFFF)

jIns :: Integer -> Integer -> Either String Instruction 
jIns opcode _ | opcode < 0 || opcode >= 2^6 = Left $ show opcode ++ " cannot be opcode"
jIns opcode addr = Right $ JIns opcode (addr .&. 0x03FFFFFF)

mnemonicToOpcode :: String -> Integer 
mnemonicToOpcode mnemonic = case mnemonic of 
    "lw"    -> 0x23
    "lb"    -> 0x20
    "lbu"   -> 0x24
    "lh"    -> 0x21 
    "lhu"   -> 0x25
    "sw"    -> 0x2b
    "sb"    -> 0x28
    "sh"    -> 0x29
    "add"   -> 0x00
    "addu"  -> 0x00
    "sub"   -> 0x00
    "subu"  -> 0x00
    "slt"   -> 0x00
    "sltu"  -> 0x00
    "and"   -> 0x00
    "or"    -> 0x00
    "xor"   -> 0x00
    "nor"   -> 0x00
    "sll"   -> 0x00
    "srl"   -> 0x00
    "sra"   -> 0x00
    "mult"  -> 0x00
    "multu" -> 0x00 
    "div"   -> 0x00
    "divu"  -> 0x00
    "addi"  -> 0x08
    "addiu" -> 0x09
    "andi"  -> 0x0c
    "ori"   -> 0x0d
    "xori"  -> 0x0e
    "lui"   -> 0x0f
    "slti"  -> 0x0a
    "sltiu" -> 0x0b
    "beq"   -> 0x04
    "bne"   -> 0x05
    "blez"  -> 0x06
    "bgtz"  -> 0x07
    "bltz"  -> 0x01
    "bgez"  -> 0x01
    "j"     -> 0x02
    "jal"   -> 0x03
    "jalr"  -> 0x00
    "jr"    -> 0x00
    "mfhi"  -> 0x00
    "mflo"  -> 0x00
    "mthi"  -> 0x00
    "mtlo"  -> 0x00
    "eret"  -> 0x10
    "mfco"  -> 0x10
    "mtco"  -> 0x10
    "break" -> 0x00
    "syscall" -> 0x00
    _       -> -1
    
mnemonicToFunct :: String -> Integer 
mnemonicToFunct mnemonic = case mnemonic of 
    "add"   -> 0x20
    "addu"  -> 0x21
    "sub"   -> 0x22
    "subu"  -> 0x23
    "slt"   -> 0x2a
    "sltu"  -> 0x2b
    "and"   -> 0x24
    "or"    -> 0x25
    "xor"   -> 0x26
    "nor"   -> 0x27
    "sll"   -> 0x00
    "srl"   -> 0x02
    "sra"   -> 0x03
    "mult"  -> 0x18
    "multu" -> 0x19 
    "div"   -> 0x1a
    "divu"  -> 0x1b
    "jalr"  -> 0x09
    "jr"    -> 0x08
    "mfhi"  -> 0x10
    "mflo"  -> 0x12
    "mthi"  -> 0x11
    "mtlo"  -> 0x13
    "eret"  -> 0x18
    "break" -> 0x0d
    "syscall" -> 0x0c


bytecode :: Instruction -> Word32
bytecode (RIns opcode (Register rs) (Register rt) (Register rd) shamt funct) = 
    fromIntegral 
         $  opcode `shiftL` 26 
        .|. rs `shiftL` 21 
        .|. rt `shiftL` 16 
        .|. rd `shiftL` 11 
        .|. shamt `shiftL` 6 
        .|. funct
bytecode (IIns opcode (Register rs) (Register rt) immed) = 
    fromIntegral
         $  opcode `shiftL` 26
        .|. rs `shiftL` 21
        .|. rt `shiftL` 16
        .|. immed 
bytecode (JIns opcode immed) = 
    fromIntegral
         $ opcode `shiftL` 26
        .|. immed 

opcodeToMnemonic :: Integer -> Either String String 
opcodeToMnemonic opcode = case opcode of 
    0x00 -> Left "Undeterministic opcode: 0x00"
    0x23 -> Right "lw"
    0x20 -> Right "lb"
    0x24 -> Right "lbu"
    0x21 -> Right "lh"
    0x25 -> Right "lhu"
    0x2b -> Right "sw"
    0x28 -> Right "sb"
    0x29 -> Right "sh"
    0x08 -> Right "addi"
    0x09 -> Right "addiu"
    0x0c -> Right "andi"
    0x0d -> Right "ori"
    0x0e -> Right "xori"
    0x0f -> Right "lui"
    0x0a -> Right "slti"
    0x0b -> Right "sltiu"
    0x04 -> Right "beq"
    0x05 -> Right "bne"
    0x06 -> Right "blez"
    0x07 -> Right "bgtz"
    0x01 -> Right "bltz"
    0x01 -> Right "bgez"
    0x02 -> Right "j"
    0x03 -> Right "jal"
    0x10 -> Right "eret"
    0x10 -> Right "mfco"
    0x10 -> Right "mtco"
    _    -> Left $ "Unknown opcode :" ++ show opcode 
    
functToMnemonic :: Integer -> Either String String 
functToMnemonic funct = case funct of 
    0x20 -> Right "add"
    0x21 -> Right "addu"
    0x22 -> Right "sub"
    0x23 -> Right "subu"
    0x2a -> Right "slt"
    0x2b -> Right "sltu"
    0x24 -> Right "and"
    0x25 -> Right "or"
    0x26 -> Right "xor"
    0x27 -> Right "nor"
    0x00 -> Right "sll"
    0x02 -> Right "srl"
    0x03 -> Right "sra"
    0x18 -> Right "mult"
    0x19 -> Right "multu"
    0x1a -> Right "div"
    0x1b -> Right "divu"
    0x09 -> Right "jalr"
    0x08 -> Right "jr"
    0x10 -> Right "mfhi"
    0x12 -> Right "mflo"
    0x11 -> Right "mthi"
    0x13 -> Right "mtlo"
    0x0d -> Right "break"
    0x0c -> Right "syscall"
    _    -> Left $ "Unknown funct: " ++ show funct

unbytecode :: Word32 -> Either String Instruction 
unbytecode word = do 
    let opcode = fromIntegral $ word `shiftR` 26 
        rs = Register $ fromIntegral $ word `shiftR` 21 .&. 0x1f
        rt = Register $ fromIntegral $ word `shiftR` 16 .&. 0x1f
        rd = Register $ fromIntegral $ word `shiftR` 11 .&. 0x1f 
        shamt = fromIntegral $ word `shiftR` 6 .&. 0x1f
        funct = fromIntegral $ word .&. 0x3f
        imm16 = immSext 16 $ fromIntegral $ word .&. 0xffff
        imm26 = immSext 26 $ fromIntegral $ word .&. 0x03ffffff
    if opcode == 0x00 
        then return $ RIns opcode rs rt rd shamt funct
        else do 
            mnemonic <- opcodeToMnemonic opcode 
            if mnemonic `elem` ["j", "jal"] 
                then return $ JIns opcode imm26 
                else return $ IIns opcode rs rt imm16
