import Data.List

spiraali :: Integer -> [[Integer]]
spiraali 1 = [[1]]
spiraali n
	| n `mod` 2 /= 0 = (zipWith (:) [i+1..i+n-1] xs) ++ [[n^2-(n-1)..n^2]]
	| otherwise = (zipWith (:) [n^2,n^2-1..n^2-(n-2)] xs) ++ [[i'+n,i'+n-1..i'+1]]
	where xs = spiraali(n-1)
	      i  = head(head xs)
	      i' = last(last xs)


main = mapM_ putStrLn $ map (\xs -> intercalate " " (map show (reverse xs))) $ spiraali 30

