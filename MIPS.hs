{-# LANGUAGE RecursiveDo #-}

module MIPS where 

import Control.Monad (ap, liftM)
import Data.Word (Word32)

type Address = Word32
type Bytecode = Word32

newtype MIPS a = MIPS { runMIPS :: Address -> ([Bytecode], Address, a) }
    
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

assemble :: MIPS a -> [Bytecode]
assemble m = bytecodes 
    where (bytecodes, _, _) = runMIPS m 0x00000000