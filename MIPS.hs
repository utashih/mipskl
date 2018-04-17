{-# LANGUAGE RecursiveDo #-}

module MIPS where 

import Control.Monad (ap, liftM)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word32)
import Prelude hiding (and, or)
import Register (Register(..), zero, at, v0, v1, 
    a0, a1, a2, a3, t0, t1, t2, t3, t4, t5, t6, t7, s0, s1, 
    s2, s3, s4, s5, s6, s7, t8, t9, k0, k1, gp, sp, fp, ra)
import Text.Printf (printf)
import Util (wordHex)

type Address = Word32
type Bytecode = Word32
type Program = [Bytecode]

newtype MIPS a = MIPS { runMIPS :: Address -> (Program, Address, a) }
    
instance Functor MIPS where 
    fmap = liftM

instance Applicative MIPS where 
    pure = return 
    (<*>) = ap

instance Monad MIPS where 
    return val = MIPS $ \addr -> ([], addr, val)
    a >>= b = MIPS $ \start -> let
        (byte0, addr0, val0) = runMIPS a start 
        (byte1, addr1, val1) = runMIPS (b val0) addr0
        in (byte0 ++ byte1, addr1, val1)

assemble :: MIPS a -> Program
assemble m = bytecodes 
    where (bytecodes, _, _) = runMIPS m 0x00000000

instance Show (MIPS a) where 
    show m = unlines (zipWith (printf "%4d  %s") [(1::Int)..] (map wordHex (assemble m)))
    
word :: Word32 -> MIPS ()
word w = MIPS $ \addr -> ([w], addr + 4, ())

label :: MIPS Address 
label = MIPS $ \addr -> ([], addr, addr)

add :: Register -> Register -> Register -> MIPS ()
add (Register rd) (Register rs) (Register rt) = word $ fromIntegral $ 
    rs `shiftL` 21 .|. rt `shiftL` 16 .|. rd `shiftL` 11 .|. 0x20

sub :: Register -> Register -> Register -> MIPS ()
sub (Register rd) (Register rs) (Register rt) = word $ fromIntegral $ 
    rs `shiftL` 21 .|. rt `shiftL` 16 .|. rd `shiftL` 11 .|. 0x22

and :: Register -> Register -> Register -> MIPS ()
and (Register rd) (Register rs) (Register rt) = word $ fromIntegral $ 
    rs `shiftL` 21 .|. rt `shiftL` 16 .|. rd `shiftL` 11 .|. 0x24

or :: Register -> Register -> Register -> MIPS ()
or (Register rd) (Register rs) (Register rt) = word $ fromIntegral $ 
    rs `shiftL` 21 .|. rt `shiftL` 16 .|. rd `shiftL` 11 .|. 0x25

addi :: Register -> Register -> Int -> MIPS ()
addi (Register rt) (Register rs) imm = word $ fromIntegral $ 
    0x08 `shiftL` 26 .|. rs `shiftL` 21 .|. rt `shiftL` 16 .|. imm .&. 0xFFFF

ori :: Register -> Register -> Int -> MIPS ()
ori (Register rt) (Register rs) imm = word $ fromIntegral $ 
    0x0d `shiftL` 26 .|. rs `shiftL` 21 .|. rt `shiftL` 16 .|. imm .&. 0xFFFF

sll :: Register -> Register -> Int -> MIPS ()
sll (Register rd) (Register rt) shamt = word $ fromIntegral $ 
    rt `shiftL` 16 .|. rd `shiftL` 11 .|. shamt `shiftL` 6 .|. 0x00

srl :: Register -> Register -> Int -> MIPS ()
srl (Register rd) (Register rt) shamt = word $ fromIntegral $ 
    rt `shiftL` 16 .|. rd `shiftL` 11 .|. shamt `shiftL` 6 .|. 0x02

lw :: Register -> Int -> Register -> MIPS ()
lw (Register rt) imm (Register rs) = word $ fromIntegral $
    0x23 `shiftL` 26 .|. rs `shiftL` 21 .|. rt `shiftL` 16 .|. imm .&. 0xFFFF

sw :: Register -> Int -> Register -> MIPS ()
sw (Register rt) imm (Register rs) = word $ fromIntegral $
    0x2b `shiftL` 26 .|. rs `shiftL` 21 .|. rt `shiftL` 16 .|. imm .&. 0xFFFF
    
lui :: Register -> Int -> MIPS () 
lui (Register rt) imm = word $ fromIntegral $
    0x0f `shiftL` 26 .|. rt `shiftL` 16 .|. imm .&. 0xFFFF

slt :: Register -> Register -> Register -> MIPS ()
slt (Register rd) (Register rs) (Register rt) = word $ fromIntegral $ 
    rs `shiftL` 21 .|. rt `shiftL` 16 .|. rd `shiftL` 11 .|. 0x2a

slti :: Register -> Register -> Int -> MIPS ()
slti (Register rt) (Register rs) imm = word $ fromIntegral $ 
    0x0a `shiftL` 26 .|. rs `shiftL` 21 .|. rt `shiftL` 16 .|. imm .&. 0xFFFF

beq :: Register -> Register -> Address -> MIPS ()
beq (Register rs) (Register rt) label = MIPS $ \addr ->
    let imm = fromIntegral label - fromIntegral (addr + 1)
        w = fromIntegral $ 0x04 `shiftL` 26 .|. rs `shiftL` 21 .|. rt `shiftL` 16 .|. (imm `shiftR` 2) .&. 0xFFFF
    in ([w], addr + 4, ())

bne :: Register -> Register -> Address -> MIPS ()
bne (Register rs) (Register rt) label = MIPS $ \addr ->
    let imm = fromIntegral label - fromIntegral (addr + 1)
        w = fromIntegral $ 0x05 `shiftL` 26 .|. rs `shiftL` 21 .|. rt `shiftL` 16 .|. (imm `shiftR` 2) .&. 0xFFFF
    in ([w], addr + 4, ())

program :: MIPS () 
program = mdo 
    add t0 s0 v0 
    sub v1 t3 a1 
    and t2 a0 t1 
    or  s1 zero s2 
    addi s3 s4 (-123)
    ori t4 t5 0x12
    sll s5 s6 10
    srl t6 t7 0x3
    lw t8 20(s7)
    sw t9 (-4)(sp)
    lui ra 21
    slt s1 s2 s3 
    slti k0 zero (-1)
    start <- label 
    beq t0 s0 start 
    bne t1 s1 start
