-- 1 задача
reverse :: [a] -> [a]
reverse arr = foldl (\acc x -> x:acc) [] arr

-- 2 задача
evenOnly :: Eq a =>[a] -> [a]
-- аккумулятор теперь кортеж в котором первый элемент массив искомых элементов, второй позиция текущего элемента в исходном массиве
evenOnly arr = fst(foldl(\acc x -> if (snd acc `mod` 2 == 0) then (fst acc++[x],snd acc +1) else (fst acc,snd acc +1)) ([],1) arr)

--3 задача
for :: (Int, a) -> (Int -> Int) -> (Int -> Bool) -> (Int -> a -> a) -> a
for (i,acc) f predic body = if predic i then  for (f i, body i acc) f predic body else acc
-- Алг. сложности равны O(n)

--4 задача
listComposition :: [a]->[b]->[(a,b)]
listComposition arr1 arr2 = [(x1,x2) | x1<- arr1, x2 <- arr2]

--5 задача
type BinaryRelation a = Eq a => [(a, a)]
refl :: Eq a => [a] -> BinaryRelation a -> Bool
refl m rel = foldl(\acc x -> if (x,x) `elem` rel then acc else False) True m
sim :: Eq a => [a] -> BinaryRelation a -> Bool
sim m rel = foldl(\ acc (x,y)->if (y,x) `elem` rel then acc else False) True rel
-- не разобрался