import Data.Char

prevChar c = chr $ ord c - 1
nextChar c = chr $ ord c + 1

porras :: Char -> Int -> Int
porras _ 1 = 1
porras 'A' x = porras 'B' (x-1)
porras 'Z' x = porras 'Y' (x-1)
porras c x = porras (prevChar c) (x-1) + porras (nextChar c) (x-1)

main = print $ sum $ map (\x -> porras x 15) ['A'..'Z']
