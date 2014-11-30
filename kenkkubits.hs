{-# LANGUAGE ScopedTypeVariables #-}
import Data.Bits

main = print $ sum $ map popCount ([1..12345678] :: [Int])
