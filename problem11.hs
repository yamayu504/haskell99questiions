-- (*) Modified run-length encoding.Solutions
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.
-- Example:
-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell:

import Data.List

-- | test
-- >>> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

data ListItem a = Multiple Int Char | Single Char
    deriving(Show)

encodeModified :: String -> [ListItem a]
encodeModified = map encodeHelper . encode
    where 
        encodeHelper (1, n) = Single n
        encodeHelper (a, n) = Multiple a n

encode :: String -> [(Int,Char)]
encode xs = map (\x -> (length x, head x)) $ group xs