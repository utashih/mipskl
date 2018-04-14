module Util where

(|>) :: a -> (a -> b) -> b 
x |>f = f x

