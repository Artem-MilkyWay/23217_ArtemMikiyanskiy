module Hw9 (stackMachine) where
import Utils
--1 задача
newtype Stack = Stack [Int]
emptyStack :: Stack
emptyStack = Stack []
--
push :: Stack -> Int -> Stack
push (Stack stackInp) elem = Stack (elem : stackInp)
--
pop :: Stack -> (Int, Stack)
pop (Stack (x:xs)) = (x, Stack xs)
-- 2 задача
data Instruction =
    Push Int
 | Add
 | Sub
 | Div
 | Mul
 | Pow
 deriving Show
add' :: Stack -> Stack
add' (Stack (x:y:stack)) = Stack ((x + y) : stack)
sub' :: Stack -> Stack
sub' (Stack (x:y:stack)) = Stack ((y - x) : stack)
div' :: Stack -> Stack
div' (Stack (x:y:stack)) = Stack ((y `div` x) : stack)
mul' :: Stack -> Stack
mul' (Stack (x:y:stack)) = Stack ((x * y) : stack)
pow' :: Stack -> Stack
pow' (Stack (x:y:stack)) = Stack ((y ^ x) : stack)
-- вспомогательная ф-ция 
compute :: [Instruction] -> Stack -> Int
compute [] (Stack [a]) = a
compute (Push x : xs) stack = compute xs (push stack x)
compute (Add : xs) stack = compute xs (add' stack)
compute (Sub : xs) stack = compute xs (sub' stack)
compute (Div : xs) stack = compute xs (div' stack)
compute (Mul : xs) stack = compute xs (mul' stack)
compute (Pow : xs) stack = compute xs (pow' stack)
--основная ф-ция
computeInstructions :: [Instruction] -> Int
computeInstructions arr = compute arr (Stack [])
--3 задача 
parse' :: String -> Instruction
parse' instr | strIsNumber instr = Push (strToInt instr)
                      | instr == "/" = Div
                      | instr == "^" = Pow
                      | instr == "+" = Add
                      | instr == "-" = Sub
                      | instr == "*" = Mul
parseString:: String -> [Instruction]
parseString str = map(\x -> parse' x) (split str ' ')

--4 задача
class Parsable a where
    parse :: a -> [Instruction]

instance Parsable String where
  parse instr = parseString instr

instance Parsable [String] where
  parse instr =  concatMap parseString instr

instance Parsable [Instruction] where
  parse instr = instr

stackMachine :: (Parsable a) => a -> Int
stackMachine instr = computeInstructions (parse instr) 