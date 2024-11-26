import Data.List

-- (*) Find the number of elements in a list.Solutions


-- Example in Haskell:

-- >>> myLength [123, 456, 789]
-- >>> 3
-- >>> myLength "Hello, world!"
-- >>> 13

myLength :: [a] -> Int
myLength  = length

myLength1 :: [a] -> Int
myLength1  [] = 0
myLength1 (_:xs) = 1 + myLength1 xs

myLength2 :: [a] -> Int
myLength2 = foldl (\n _ -> n + 1) 0

-- 解答

myLength3 :: [a] -> Int
myLength3 = sum . map (const 1) 
-- constで常に左側を返す。mapでconst x y のyの方にリストの値を適用する。