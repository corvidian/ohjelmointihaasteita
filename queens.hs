#Shakissa kuningattaret uhkaavat toisiaan, jos ne ovat samalla pysty-, vaaka- tai vinorivillä. Tehtäväsi on selvittää, monellako tavalla annetun kokoiselle shakkilaudalle voi sijoittaa kaksi kuningatarta niin, etteivät ne uhkaa toisiaan.

#Esimerkiksi jos shakkilaudan koko on 3x3, tapoja on 8:

#Ilmoita sijoitustapojen määrä, kun shakkilaudan koko on 50x50.

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
