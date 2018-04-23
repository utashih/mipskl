module Register where 

import Control.Monad (guard)
import Data.Char (toLower)
import Text.Read (readMaybe)

newtype Register = Register Integer deriving (Eq)

register :: String -> Maybe Register
register ('$':name) = do
    let ret = return . Register
    case map toLower name of 
        "zero" -> ret 0
        "at"   -> ret 1
        "v0"   -> ret 2
        "v1"   -> ret 3
        "a0"   -> ret 4
        "a1"   -> ret 5
        "a2"   -> ret 6
        "a3"   -> ret 7
        "t0"   -> ret 8
        "t1"   -> ret 9
        "t2"   -> ret 10
        "t3"   -> ret 11
        "t4"   -> ret 12
        "t5"   -> ret 13
        "t6"   -> ret 14
        "t7"   -> ret 15
        "s0"   -> ret 16
        "s1"   -> ret 17
        "s2"   -> ret 18
        "s3"   -> ret 19
        "s4"   -> ret 20
        "s5"   -> ret 21
        "s6"   -> ret 22
        "s7"   -> ret 23
        "t8"   -> ret 24
        "t9"   -> ret 25
        "k0"   -> ret 26
        "k1"   -> ret 27
        "gp"   -> ret 28
        "sp"   -> ret 29
        "fp"   -> ret 30
        "ra"   -> ret 31
        _      -> do 
            idx <- readMaybe name
            guard (idx >= 0)
            guard (idx < 32)
            guard (show idx == name)
            ret idx
register _ = Nothing

idxToName :: Integer -> String 
idxToName idx = case idx of 
    0  -> "$zero"
    1  -> "$at"
    2  -> "$v0"
    3  -> "$v1"
    4  -> "$a0" 
    5  -> "$a1"
    6  -> "$a2"
    7  -> "$a3"
    8  -> "$t0"
    9  -> "$t1"
    10 -> "$t2"
    11 -> "$t3"
    12 -> "$t4"
    13 -> "$t5"
    14 -> "$t6"
    15 -> "$t7"
    16 -> "$s0"
    17 -> "$s1"
    18 -> "$s2"
    19 -> "$s3"
    20 -> "$s4"
    21 -> "$s5"
    22 -> "$s6"
    23 -> "$s7"
    24 -> "$t8"
    25 -> "$t9"
    26 -> "$k0"
    27 -> "$k1"
    28 -> "$gp"
    29 -> "$sp"
    30 -> "$fp"
    31 -> "$ra"
    _  -> "$WrongRegisterIndex"

instance Show Register where 
    show (Register idx) = idxToName idx ++ show [idx]

zero, at, v0, v1, a0, a1, a2, a3 :: Register
t0, t1, t2, t3, t4, t5, t6, t7 :: Register
s0, s1, s2, s3, s4, s5, s6, s7 :: Register
t8, t9, k0, k1, gp, sp, fp, ra :: Register
zero = Register 0
at   = Register 1
v0   = Register 2
v1   = Register 3
a0   = Register 4
a1   = Register 5
a2   = Register 6
a3   = Register 7
t0   = Register 8
t1   = Register 9
t2   = Register 10
t3   = Register 11
t4   = Register 12
t5   = Register 13
t6   = Register 14
t7   = Register 15
s0   = Register 16
s1   = Register 17
s2   = Register 18
s3   = Register 19
s4   = Register 20
s5   = Register 21
s6   = Register 22
s7   = Register 23
t8   = Register 24
t9   = Register 25
k0   = Register 26
k1   = Register 27
gp   = Register 28
sp   = Register 29
fp   = Register 30
ra   = Register 31
