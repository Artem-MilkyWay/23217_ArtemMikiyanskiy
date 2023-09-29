-- задача №1
f :: Double -> Double
f p = p*2
g :: Double-> Double
g p = p*3
funcmax :: (Double -> Double)->(Double->Double)->Double->Double
funcmax f g p | f(p)>g(p) = f(p)
                       | g(p)>f(p) = g(p)
-- задача №2
e :: Double
e = exp 1
funcmax2 :: (Double -> Double)->Double->Double
funcmax2 f p = funcmax f (\p -> e*p) p 
-- задача №3
func3 :: (Double -> Double) -> Double -> Int -> Double
func3 f p n | n<0 = 0
                   | n==0 = p
                   | otherwise = func3 f (f(p)) (n-1)
