import System.Environment

q :: Integer -> Integer
q n = q' n 0
 where
  q' 2 x = x
  q' n x = q' (n-1) (x + (reunaPaikkoja * (ruutuja - varaamatRuudut) - symmetriset))
    where reunaPaikkoja = 2*n - 1
          ruutuja = n^2
          varaamatRuudut = 3*(n-1) + 1
          symmetriset = (n - 1)*(n - 2)


main = do
	args <- getArgs
	let n = read $ head args
	putStrLn $ show $ q n
