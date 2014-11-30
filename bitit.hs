-- bittejä jonossa 1..(2^n-1)
l :: Integer -> Integer
l 1 = 1
l n = 2 * l (n-1) + 2^(n-1)

bitit :: Integer -> Integer
bitit 0 = 0
bitit 1 = 1
bitit n = l maxPow + n - 2^maxPow + 1 + bitit (n - 2^maxPow)
	where maxPow = floor $ logBase 2 (fromIntegral n) :: Integer

main = print $ bitit (10^16)
