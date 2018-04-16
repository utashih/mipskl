module Register where 

import Text.Read (readMaybe)

newtype Register = Reg Int deriving (Show, Eq)

regIndex :: String -> Maybe Register
regIndex ('$':name) = do 
    idx <- readMaybe name
    return $ Reg idx
regIndex _ = Nothing
