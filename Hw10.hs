data BinTree a =
  Nil |
  Node {
    left :: BinTree a,
    right :: BinTree a,
    value :: a,
    count :: Int
  }
-- для более удобного вывода
instance Show a => Show (BinTree a) where
  show = show0 0 where
    show0 _ Nil = "Nil"
    show0 lvl Node{left=l, right=r, value=v, count=cnt} =
      "Node (v = " ++ show v ++ ")\n" ++
      replicate lvl '\t' ++ "l=" ++ show0 (lvl+1) l ++ "\n" ++
      replicate lvl '\t' ++ "r=" ++ show0 (lvl+1) r ++ "\n"

testTree :: BinTree Int
testTree = Node {
    left = Node {
        left = Node {left  = Nil, right = Nil, value = 1, count = 1},
        right = Node {
            left = Node {
                left = Nil, right = Nil, value = 4, count = 1},
            right = Node {
                left = Nil, right = Nil, value = 7, count = 1},
            value = 6, count = 1
        },
        value = 3, count = 1
    },
    right = Node {
        left  = Nil,
        right = Node {
            left  = Node {
                left = Nil, right = Nil, value = 13, count = 1},
            right = Nil,
            value = 14, count = 1
        },
        value = 10, count = 1
    },
    value = 8, count = 1
}

-- 1 задача
insert :: Ord a => BinTree a -> a -> BinTree a
insert Nil elem = Node Nil Nil elem 1
insert (Node left right value count) elem  | elem < value  = Node (insert left elem) right value count
                                                                  | elem > value = Node left (insert right elem) value count
                                                                  | otherwise  = Node left right value (count + 1)
fromList :: Ord a => [a] -> BinTree a
fromList arr = foldl(\acc x ->insert acc x) Nil arr 
--2 задача

findMin :: Ord a => BinTree a -> Maybe a 
findMin (Node Nil right value count ) = Just value
findMin (Node left right value count) = findMin (left)   

findMax :: Ord a => BinTree a -> Maybe a 
findMax (Node left Nil value count ) = Just value
findMax (Node left right value count) = findMin (right)

-- 4 задача 
findAny :: Ord a => (a -> Bool) -> BinTree a -> Maybe a
findAny predicate (Node left right value count) | predicate value = Just value
                                                                            | 1 == 1 = case (left, right) of 
                                                                                (Nil, Nil) -> Nothing -- если обе ветки пустые 
                                                                                (Nil, _) -> foldl(\acc x -> if x/= Nothing then x else acc) Nothing ([findAny predicate right]) -- если левая ветка пустая
                                                                                (_, Nil) -> foldl(\acc x -> if x/= Nothing then x else acc) Nothing ([findAny predicate left]) -- если правая ветка пустая
                                                                                _ -> foldl(\acc x -> if x/= Nothing then x else acc) Nothing ([findAny predicate left]++[findAny predicate right])   -- если обе ветки не пустые                                                          
