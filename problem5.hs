-- (*) Reverse a list.Solutions
-- Example in Haskell:

-- |加算を行う関数
-- >>> myReverse [1,2,3,4]
-- [4,3,2,1]
-- >>> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- >>> myReverse1 [1,2,3,4]
-- [4,3,2,1]
-- >>> myReverse1 "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse1 :: [a] -> [a]
myReverse1 = foldl (\acc a -> a:acc) [] 
