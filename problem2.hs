
-- (*) Find the last-but-one (or second-last) element of a list.Solutions
-- (Note that the Lisp transcription of this problem is incorrect.)
-- Example in Haskell:

--- >>> myButLast [1,2,3,4]
--- >>> 3
--- >>> myButLast ['a'..'z']
--- >>> 'y'

myButLast :: [a] -> a
myButLast [] = error "No end for empty lists!"
myButLast [x] = error "No end for empty lists!"
myButLast (x:xs) =
        if length xs == 1 
            then x
            else myButLast xs

myButLast1 :: [a] -> a
myButLast1 = head . tail . reverse


--- 解答

myButLast2 [x, _] = x -- ここで２つの要素しかないパターンマッチング
myButLast2 (_:xs) = myButLast2 xs