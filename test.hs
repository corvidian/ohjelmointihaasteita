double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

last' ns = head (reverse ns)
last'' ns = ns !! (length ns - 1)

n = a `div` length xs
	where
		a = 10
		xs = [1,2,3,4,5]

init' ns = reverse (tail (reverse ns))
init'' ns = take (length ns - 1) ns

qsort [] = []
qsort (x : xs) = qsort larger ++ [x] ++  qsort smaller
	where
		larger = [a | a <- xs, a > x || a == x]
		smaller = [b | b <- xs, b < x]

second xs = head (tail xs)
palindrome xs = reverse xs == xs
twice f x = f (f x)
f xs = take 3 (reverse xs)
e3 x = x * 2
