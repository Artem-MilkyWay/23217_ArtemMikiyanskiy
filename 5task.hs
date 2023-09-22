-- 5 задача
summ :: Int -> Int
summ x | x<10 = x
          | otherwise = x`mod`10+summ(x`div`10)
magic :: Int -> Int
magic x | x<10 = x
             | otherwise = magic(summ(x))