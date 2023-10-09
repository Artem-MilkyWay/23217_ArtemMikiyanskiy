-- 1 задача
map' :: (a -> b) -> [a] ->[b]
map'  f a = foldl (\acc x->acc++[f x]) [] a
--2 задача
nub' :: Eq a => [a]->[a]
nub' [] = []
nub' (x:xs) = x : nub' (filter(\z -> z/=x) xs)

union :: Eq a => [a] -> [a] -> [a]
union a1 a2 = nub' (a1++a2)

intersection :: Eq a => [a]->[a]->[a]
intersection a1 a2 = filter (`elem` a1) a2 
--3 задача
compute :: [String]->Double -> Double
compute [] p = p
compute (x:xs) p | x=="inc" = compute xs (p+1)
                           | x == "dec" = compute xs (p-1)
                           | x == "double" = compute xs (p*2)
                           | x == "sqrt" = compute xs (sqrt p)
                           | (x == "halveIfPositive") = if p>0 then compute xs (p/2) else p
                           | otherwise = p
computeVector :: [String]-> [Double] -> [Double]
computeVector commands arr= foldl(\acc x -> acc++[compute commands x]) [] arr
--4 задача
commands = ["inc","dec","double","sqrt","halveIfPositive"]
cleaner :: [String]->[String]
cleaner allCommands= filter(`elem`commands) allCommands
--5 задача
optimizer :: [String] -> [String]
optimizer [] = []
optimizer (x:xs) = if (x=="dec") && (xs!!0=="inc") || (x=="inc") && (xs!!0=="dec") then optimizer (tail xs) else x : optimizer xs 
--6 задача
func :: (Double -> Double) ->[(Double,Double)]->[(Double,Double)]
func f points = filter(\x ->f (fst x) < snd x) points