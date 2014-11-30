import Data.List

jono :: Int -> String -> String
jono a s = take (a*a) (cycle s)

nelio :: Int -> String -> [String]
nelio _ [] = []
nelio a xs = (take a xs) : nelio a (drop a xs)

main = do 
	putStrLn "Sana: "
	sana <- getLine
	putStrLn "Neliön sivun pituus: "
	leveysString <- getLine
	let leveys = read leveysString
	mapM_ putStrLn $ transpose (nelio leveys $ jono leveys sana)
