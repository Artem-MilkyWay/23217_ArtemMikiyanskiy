-- 1 задача
fibonacci :: Int -> Int
fibonacci n | n ==1 = 1
                  | n ==2 = 1
                  | otherwise = fibonacci(n-1)+fibonacci(n-2)
fibMod5 :: [Int]
fibMod5 = [fibonacci(x) | x <- [1..],fibonacci(x)`mod`5==0]
-- 2 задача
perimetr :: [(Double, Double)] -> Double
perimetr [(x1,y1), (x2,y2), (x3,y3)] = (sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))) + (sqrt((x1-x3)*(x1-x3)+(y1-y3)*(y1-y3))) + (sqrt((x2-x3)*(x2-x3)+(y2-y3)*(y2-y3)))
--3 задача
checkAllEq :: Eq a => [a] -> Bool
checkAllEq arr = if length arr <= 1 then True else if arr!!0 == arr!!1 then checkAllEq(tail arr) else False
--4 задача
distance:: (Double,Double) -> (Double,Double) -> Double
distance (x1,y1) (x2,y2)= sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
minDistance :: [(Double,Double)] -> Double
minDistance allPoints = minimum[distance (x1,y1) (x2,y2) |(x1,y1) <- allPoints, (x2,y2) <- allPoints, (x1,y1)/=(x2,y2)] 
--5 задача
compute :: [String]->Double -> Double
compute [] p = p
compute (x:xs) p | x=="inc" = compute xs (p+1)
                           | x == "dec" = compute xs (p-1)
                           | x == "double" = compute xs (p*2)
                           | x == "sqrt" = compute xs (sqrt p)
                           | (x == "halveIfPositive") = if p>0 then compute xs (p/2) else p
                           | otherwise = p