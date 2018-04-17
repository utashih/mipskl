{-# LANGUAGE RecursiveDo #-}

module MIPS where 

import Control.Monad (ap, liftM)
import Data.Word (Word32)
import Register (Register, zero, at, v0, v1, 
    a0, a1, a2, a3, t0, t1, t2, t3, t4, t5, t6, t7, s0, s1, 
    s2, s3, s4, s5, s6, s7, t8, t9, k0, k1, gp, sp, fp, ra)

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

word :: Word32 -> MIPS ()
word w = MIPS $ \addr -> ([w], addr + 1, ())

label :: MIPS Address 
label = MIPS $ \addr -> ([], addr, addr)

assemble :: MIPS a -> Program
assemble m = bytecodes 
    where (bytecodes, _, _) = runMIPS m 0x00000000
