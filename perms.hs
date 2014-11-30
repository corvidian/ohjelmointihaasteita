import Data.List

main = mapM_ putStrLn $ map (\xs -> concat $ map show xs) $ sort $ permutations [1..6]
