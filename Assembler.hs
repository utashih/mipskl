module Assembler where 

import Control.Monad (forM_)   
import Data.Bits ((.&.), (.|.), shiftL, shiftR) 
import Data.Word (Word32)
import Parser (ASTExpr(..), ASTInstruction(..), ASTLine(..), parseASM)
import Register (Register(..), register)
import Text.Printf (printf)
import Util (wordHex)

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

assemble :: ASTInstruction -> Either String Instruction
assemble (AITen (AESym mnemonic) (AEReg rd) (AEReg rs) (AEReg rt)) = 
    let opcode = mnemonicToOpcode mnemonic
        funct = mnemonicToFunct mnemonic
    in case mnemonic of 
        "add"   -> rIns opcode rs rt rd 0 funct
        "sub"   -> rIns opcode rs rt rd 0 funct
        "and"   -> rIns opcode rs rt rd 0 funct
        "or"    -> rIns opcode rs rt rd 0 funct
        "slt"   -> rIns opcode rs rt rd 0 funct
        _       -> Left $ "Unsupported mnemonic: <" ++ mnemonic ++ ">"
assemble (AITen (AESym mnemonic) (AEReg rt) (AEReg rs) (AEImm immed)) =
    let opcode = mnemonicToOpcode mnemonic
        funct = mnemonicToFunct mnemonic
    in case mnemonic of 
        "addi"  -> iIns opcode rs rt immed
        "ori"   -> iIns opcode rs rt immed 
        "sll"   -> rIns opcode "$zero" rs rt immed funct
        "srl"   -> rIns opcode "$zero" rs rt immed funct
        "slti"  -> iIns opcode rs rt immed 
        _       -> Left $ "Unsupported mnemonic: <" ++ mnemonic ++ ">"
assemble (AITen (AESym mnemonic) (AEReg rs) (AEReg rt) (AESym label)) = 
    let opcode = mnemonicToOpcode mnemonic
    in case mnemonic of  
        "beq"   -> Left "Unsupported mnemonic: <beq>"
        "bne"   -> Left "Unsupported mnemonic: <bne>"
        _       -> Left $ "Unsupported mnemonic: <" ++ mnemonic ++ ">"
assemble (AIOff (AESym mnemonic) (AEReg rt) (AEImm immed) (AEReg rs)) = 
    let opcode = mnemonicToOpcode mnemonic
    in case mnemonic of 
        "lw"    -> iIns opcode rs rt immed 
        "sw"    -> iIns opcode rs rt immed 
        _       -> Left $ "Unsupported mnemonic: <" ++ mnemonic ++ ">"
assemble (AIBin (AESym mnemonic) (AEReg rt) (AEImm immed)) = 
    let opcode = mnemonicToOpcode mnemonic
    in case mnemonic of 
        "lui"   -> iIns opcode "$zero" rt immed 
        _       -> Left $ "Unsupported mnemonic: <" ++ mnemonic ++ ">"
assemble (AIJmp (AESym mnemonic) (AESym label)) =
    let opcode = mnemonicToOpcode mnemonic
    in case mnemonic of 
        "j"     -> Left "Unsupported mnemonic: <j>"
        "jal"   -> Left "Unsupported mnemonic: <jal>"
        _       -> Left $ "Unsupported mnemonic: <" ++ mnemonic ++ ">"
assemble (AIJmp (AESym mnemonic) (AEReg rs)) =
    let opcode = mnemonicToOpcode mnemonic
        funct = mnemonicToFunct mnemonic
    in case mnemonic of 
        "jr"    -> rIns opcode rs "$zero" "$zero" 0 funct
        _       -> Left $ "Unsupported mnemonic: <" ++ mnemonic ++ ">"
assemble _ = Left "Ill-formatted instruction"


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

test :: String -> Either String Instruction
test src = 
    let Right (ALInstruction ins:_) = parseASM src
    in  assemble ins

test2 :: Instruction -> (Instruction, String)
test2 ins = (ins, wordHex $ bytecode ins)

testcases :: [String]
testcases = [
    "add $t0, $s0, $v0",
    "sub $v1, $t3, $a1",
    "and $t2, $a0, $t1",
    "or  $s1, $zero, $s2",
    "addi $s3, $s4, -123",
    "ori $t4, $t5, 0x12",
    "sll $s5, $s6, 10",
    "srl $t6, $t7, 0x3",
    "lw $t8, 20($s7)",
    "sw $t9, -4($sp)",
    "lui $ra, 21",
    "slt $s1, $s2, $s3",
    "slti $k0, $zero, -1",
    "beq $t0, $s0, start",
    "bne $t1, $s1, start",
    "j start",
    "jal start",
    "jr $ra"]



runTest :: IO ()
runTest = do 
    putStrLn "Running Tests"
    forM_ (zip [(1::Integer)..] testcases)  $ \(no, src) -> do 
        printf "%2d " no
        print $ test2 <$> test src


