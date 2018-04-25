module OutputCOE where 

import Data.Word (Word32)
import Data.List (intercalate)
import Util (wordHex)

headText :: String 
headText = "memory_initialization_radix=16; \nmemory_initialization_vector=\n"

composeCoe :: [Word32] -> String 
composeCoe bytecode = headText ++ intercalate ",\n" (map wordHex bytecode) ++ ";\n"
