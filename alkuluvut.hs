alut = [23,19,17,13,11,7,5,3,2]

luvut 0 = 1
luvut x = sum $ map (\t -> luvut (x-t)) $ filter (<= x) alut

main = print $ luvut 25
