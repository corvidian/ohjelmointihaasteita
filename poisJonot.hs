--Tehtäväsi on laskea, montako erilaista merkkijonoa voi muodostaa poistamalla annetusta
--merkkijonosta osan merkeistä. Jäljelle jäävien merkkien täytyy olla samassa järjestyksessä
--kuin alkuperäisessä merkkijonossa.

--Esimerkiksi merkkijonosta LAAJA saa 18 merkkijonoa: A, J, L, AA, AJ, JA, LA, LJ, AAA, AAJ,
--AJA, LAA, LAJ, LJA, AAJA, LAAA, LAAJ ja LAJA.

--Kirjoita vastaukseen, montako merkkijonoa saa merkkijonosta SAIPPUAKAUPPIAS.

import qualified Data.Set as S

strs s = map (\x -> let (ys,zs) = splitAt x s in ys ++ (tail zs)) [0..(length s - 1)]

jonot set "" = set
jonot set s = if (s `S.member` set) 
                then set
                else S.insert s $ foldl jonot set $ strs s

main = print $ S.size (jonot S.empty "SAIPPUAKAUPPIAS") - 1
