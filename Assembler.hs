module Assembler where 

import Control.Monad (forM, forM_)   
import Instruction (Instruction(..), rIns, iIns, jIns, 
                    mnemonicToOpcode, mnemonicToFunct, bytecode)
import Parser (ASTExpr(..), ASTInstruction(..), ASTLine(..), parseASM)
import Text.Printf (printf)
import Util (partialSum, wordHex)


insLength :: [ASTLine] -> Either String [Integer]
insLength lns = forM lns $ \line -> case line of 
    ALLabel _ -> Right 0
    ALInstruction aIns -> do 
        ins <- assembles aIns 
        return $ fromIntegral $ length ins
    ALLabelledInstruction _ aIns -> do
        ins <- assembles aIns
        return $ fromIntegral $ length ins

lineno :: [ASTLine] -> Either String [Integer]
lineno = fmap partialSum . insLength 

makeSymbolTable :: [ASTLine] -> Either String [(String, Integer)]
makeSymbolTable lns = do
    linenos <- lineno lns 
    return $ traverseLines (zip lns linenos)
      where --traverseLines :: [(ASTLine, Integer)] -> [(String, Integer)]
        traverseLines [] = []
        traverseLines ((line, no) : lines) = case line of 
            ALLabel (AESym label) -> (label, no) : traverseLines lines 
            ALLabelledInstruction (AESym label) _ -> (label, no) : traverseLines lines 
            ALInstruction _ -> traverseLines lines


assemble :: ASTInstruction -> [Either String Instruction]
assemble (AITen (AESym mnemonic) (AEReg rd) (AEReg rs) (AEReg rt)) = 
    let opcode = mnemonicToOpcode mnemonic
        funct = mnemonicToFunct mnemonic
    in case mnemonic of 
        "add"   -> [rIns opcode rs rt rd 0 funct]
        "sub"   -> [rIns opcode rs rt rd 0 funct]
        "and"   -> [rIns opcode rs rt rd 0 funct]
        "or"    -> [rIns opcode rs rt rd 0 funct]
        "slt"   -> [rIns opcode rs rt rd 0 funct]
        _       -> [Left $ "Unsupported tenary mnemonic: <" ++ mnemonic ++ ">"]
assemble (AITen (AESym mnemonic) (AEReg rt) (AEReg rs) (AEImm immed)) =
    let opcode = mnemonicToOpcode mnemonic
        funct = mnemonicToFunct mnemonic
    in case mnemonic of 
        "addi"  -> [iIns opcode rs rt immed]
        "ori"   -> [iIns opcode rs rt immed]
        "sll"   -> [rIns opcode "$zero" rs rt immed funct]
        "srl"   -> [rIns opcode "$zero" rs rt immed funct]
        "slti"  -> [iIns opcode rs rt immed]
        _       -> [Left $ "Unsupported tenary mnemonic: <" ++ mnemonic ++ ">"]
assemble (AITen (AESym mnemonic) (AEReg rs) (AEReg rt) (AESym label)) = 
    let opcode = mnemonicToOpcode mnemonic
    in case mnemonic of  
        "beq"   -> [Left "Unsupported mnemonic: <beq>"]
        "bne"   -> [Left "Unsupported mnemonic: <bne>"]
        _       -> [Left $ "Unsupported tenary mnemonic: <" ++ mnemonic ++ ">"]
assemble (AIOff (AESym mnemonic) (AEReg rt) (AEImm immed) (AEReg rs)) = 
    let opcode = mnemonicToOpcode mnemonic
    in case mnemonic of 
        "lw"    -> [iIns opcode rs rt immed]
        "sw"    -> [iIns opcode rs rt immed]
        _       -> [Left $ "Unsupported mnemonic with offset: <" ++ mnemonic ++ ">"]
assemble (AIBin (AESym mnemonic) (AEReg rt) (AEImm immed)) = 
    let opcode = mnemonicToOpcode mnemonic
    in case mnemonic of 
        "lui"   -> [iIns opcode "$zero" rt immed]
        _       -> [Left $ "Unsupported binary mnemonic: <" ++ mnemonic ++ ">"]
assemble (AIJmp (AESym mnemonic) (AESym label)) =
    let opcode = mnemonicToOpcode mnemonic
    in case mnemonic of 
        "j"     -> [Left "Unsupported mnemonic: <j>"]
        "jal"   -> [Left "Unsupported mnemonic: <jal>"]
        _       -> [Left $ "Unsupported unary mnemonic: <" ++ mnemonic ++ ">"]
assemble (AIJmp (AESym mnemonic) (AEReg rs)) =
    let opcode = mnemonicToOpcode mnemonic
        funct = mnemonicToFunct mnemonic
    in case mnemonic of 
        "jr"    -> [rIns opcode rs "$zero" "$zero" 0 funct]
        _       -> [Left $ "Unsupported unary mnemonic: <" ++ mnemonic ++ ">"]
assemble _ = [Left "Ill-formatted instruction"]

assembles :: ASTInstruction -> Either String [Instruction]
assembles = sequence . assemble


test :: String -> Either String [Instruction]
test src = 
    let Right (ALInstruction ins:_) = parseASM src
    in  assembles ins

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
        print $ fmap test2 <$> test src


