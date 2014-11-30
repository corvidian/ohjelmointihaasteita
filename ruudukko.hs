import Data.Char

replace a b c = if (a == c) then b else c

propagate xs = map (replace (head xs) (chr $ ord (head xs) - 1)) xs

palindrome xs = reverse (tail xs) ++ xs

main = mapM_ putStrLn $ palindrome $ map palindrome $ take n (iterate propagate startString)
	where n = 26
	      startString = reverse $ take n ['A'..'z']
