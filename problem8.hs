-- (**) Eliminate consecutive duplicates of list elements.Solutions
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
-- Example:
-- * (compress '(a a a a b c c a a d e e e e))
-- (A B C A D E)
-- Example in Haskell:
import Data.List


-- | test
-- >>> compress "aaaabccaadeeee"
-- "abcade"
-- >>> compress' "aaaabccaadeeee"
-- "abcade"
-- >>> compress1 "aaaabccaadeeee"
-- "abcade"
-- >>> compress2 "aaaabccaadeeee"
-- "abcade"

compress :: String -> String
compress []  = []
compress [x] = [x]
compress (x:y:xs) = if x == y
  then compress (y:xs)
  else x: compress (y:xs)

compress' :: String -> String
compress' (x:y:xs)
  | x == y = compress (y:xs)
  | otherwise = x: compress (y:xs)
-- ガードで書いたほうがifがなくてかっこいい。

compress1 :: String -> String
compress1 xs = map head $ group xs

compress2 :: String -> String
compress2  xs = reverse $ foldl (\acc x -> if head acc == x then acc else x:acc) [head xs] xs
-- []の中にhead xを先に入れておくことで初期値としている。
