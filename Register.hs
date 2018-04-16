module Register where 

import Control.Monad (guard)
import Data.Char (toLower)
import Text.Read (readMaybe)

newtype Register = Register Int deriving (Show, Eq)

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
