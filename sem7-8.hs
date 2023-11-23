import Data.Array

newtype Matrix = Matrix (Array (Int, Int) Double) deriving Show


example2 :: [((Int, Int), Double)]
example2 = [((0,0), 13), ((0,1), 0),
            ((1,0), 0 ), ((1,1), 12)]

m2 = makeMatrix (2, 2) example2

example1 :: [((Int, Int), Double)]
example1 = [((0,0), 1), ((0,1), -2), ((0,2), 3),
            ((1,0), 4), ((1,1), 0), ((1,2), 6),
            ((2,0), -7), ((2,1), 8), ((2,2), 9),
            ((2,0), -7), ((2,1), 8), ((2,2), 9)]

m1 = makeMatrix (3, 4) example1

-- Задание 1

makeMatrix :: (Int, Int) -> [((Int, Int), Double)] -> Matrix
makeMatrix sizeM elementsM = Matrix (array ((0, 0), (fst(sizeM)-1, snd(sizeM)-1)) elementsM)

(!!!) :: Matrix -> (Int, Int) -> Double
(!!!) (Matrix matrix') crd = matrix' ! (fst(crd), snd(crd))

matrixSize :: Matrix -> (Int, Int)
matrixSize (Matrix matrix') = (fst (snd (bounds matrix')) + 1, snd (snd (bounds matrix')) + 1 )

-- Задание 5

instance Eq Matrix where
    (Matrix a) == (Matrix b) = a == b