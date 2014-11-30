main = interact lukija

lukija :: String -> String
lukija s = 
	let allLines = lines s
	    result = unlines $ map (lukija') (map read allLines :: [Int])
	in result

lukija' :: Int -> String
lukija' n
	| n == 0	= ""
	| n < 10	= lukusanat !! n
	| n == 10	= "kymmenen"
	| n < 20	= lukija' (n-10) ++ "toista"
	| n < 100	= lukija' (n `div` 10) ++ "kymmentä" ++ lukija' (n `mod` 10)
	| n < 200	= "sata" ++ lukija' (n `mod` 100)
	| n < 1000	= lukija' (n `div` 100) ++ "sataa" ++ lukija' (n `mod` 100)
	| n < 2000 = "tuhat" ++ lukija' (n `mod` 1000)
	| n < 1000000 = lukija' (n `div` 1000) ++ "tuhatta" ++ lukija' (n `mod` 1000)
	| n < 2000000 = "miljoona" ++ lukija' (n `mod` 1000000)
	| n < 1000000000 = lukija' (n `div` 1000000) ++ "miljoonaa" ++ lukija' (n `mod` 1000000)
	| otherwise = "Lots."
	where lukusanat = ["nolla", "yksi", "kaksi", "kolme", "neljä", "viisi",
					   "kuusi", "seitsemän", "kahdeksan", "yhdeksän"]
