--Huomataan, että riippumatta numeroiden lukumäärästä, ykköset loppuvat aina ensin
import Data.List
import Data.String

--viimeinen :: Int -> Int -> Int -> Int
viimeinen raja ykkösiä luku
	| uudetYkköset <= raja = viimeinen raja uudetYkköset (luku + 1)
	| otherwise = luku - 1
	where uudetYkköset = ykkösiä + (length $ elemIndices '1' $ show luku)
main = putStrLn $ show $ viimeinen 99999 0 1 
