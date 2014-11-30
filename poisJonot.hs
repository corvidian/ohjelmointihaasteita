import qualified Data.Set as S

strs s = map (\x -> let (ys,zs) = splitAt x s in ys ++ (tail zs)) [0..(length s - 1)]

jonot set "" = set
jonot set s = if (s `S.member` set) 
                then set
                else S.insert s $ foldl jonot set $ strs s

main = print $ S.size (jonot S.empty "SAIPPUAKAUPPIASAS") - 1
