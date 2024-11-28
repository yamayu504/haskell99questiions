-- (**) Decode a run-length encoded list.Solutions
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
-- Example in Haskell:

-- | test
-- >>> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
-- >>> decodeModified1 [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

data ListItem a = Multiple Int Char | Single Char
    deriving(Show)

decodeModified :: [ListItem a] -> String
decodeModified  = concatMap decodeHelper
    where
        decodeHelper (Multiple a b) = replicate a b
        decodeHelper (Single b) = [b]

decodeModified1 :: [ListItem a] -> String
decodeModified1  = foldl (\acc x -> acc ++ decodeHelper x) []
    where
        decodeHelper (Multiple a b) = replicate a b
        decodeHelper (Single b) = [b]

-- 解答

decodeModified2 :: Eq a => [(Int,a)] -> [a]
decodeModified2 = foldr f []
  where
    f (1, x) r = x : r
    f (k, x) r = x : f (k-1, x) r
-- パターンマッチだけで成立させている