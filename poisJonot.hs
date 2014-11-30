#Tehtäväsi on laskea, monellako tavalla annetun luvun voi muodostaa alkulukujen summana. Jos kahdessa summassa on samat luvut eri järjestyksessä, ne lasketaan erikseen. Myös yhden luvun sisältävä summa lasketaan mukaan.

import qualified Data.Set as S

strs s = map (\x -> let (ys,zs) = splitAt x s in ys ++ (tail zs)) [0..(length s - 1)]

jonot set "" = set
jonot set s = if (s `S.member` set) 
                then set
                else S.insert s $ foldl jonot set $ strs s

main = print $ S.size (jonot S.empty "SAIPPUAKAUPPIASAS") - 1
