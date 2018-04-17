{-# LANGUAGE RecursiveDo #-}

module MIPS where 

import Control.Monad (ap, liftM)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word (Word32)
import Prelude hiding (and, or)
import Register (Register(..), zero, at, v0, v1, 
    a0, a1, a2, a3, t0, t1, t2, t3, t4, t5, t6, t7, s0, s1, 
    s2, s3, s4, s5, s6, s7, t8, t9, k0, k1, gp, sp, fp, ra)
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
    show = unlines . map wordHex . assemble
    
word :: Word32 -> MIPS ()
word w = MIPS $ \addr -> ([w], addr + 1, ())

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

lw :: Register -> Int -> Register -> MIPS ()
lw (Register rt) imm (Register rs) = word $ fromIntegral $
    0x23 `shiftL` 26 .|. rs `shiftL` 21 .|. rt `shiftL` 16 .|. imm .&. 0xFFFF


program :: MIPS () 
program = do 
    add t0 s0 v0 
    sub v1 t3 a1 
    and t2 a0 t1 
    or  s1 zero s2 
    addi s3 s4 (-123)
    ori t4 t5 0x12
    
