
-- (*) Find the last element of a list.Solutions
-- (Note that the Lisp transcription of this problem is incorrect.)
-- Example in Haskell:

--
-- >>> myLast [1,2,3,4]
-- >>> 4
-- >>> myLast ['x','y','z']
-- >>>'z'
--

myList :: [a] -> a
myList [] =  error "No end for empty lists!"
myList (x:xs) = if null xs
    then x 
    else myList xs

myList1 :: [a] -> a
myList1 = head . reverse

myList2 :: [a] -> a
myList2 = last

--- 解答メモ

myList3 :: [a] -> a
myList3 = foldr1 (const id)

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x -- ここが要素一つの場合を指している。
myLast (_:xs) = myLast xs
