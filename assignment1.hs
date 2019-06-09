myLast a = last a
myReverse a = reverse a
isPalindrome a = (if reverse a == a then True else False)
compress (x:xs) = if xs == [] then x:[] else ( if(x == head xs) then (compress xs) else (x:compress xs) )
dupli a = [head a,head a]++ (if length a == 1 then [] else dupli (drop 1 a))

length' xs = fromIntegral (length xs)
rotate xs x = if x > 0 then (lastPart xs x ++ take x xs ) else lastPart xs (x +length' xs) ++  take (x +length' xs) xs
lastPart xs x = if x == 0 then xs else (lastPart (tail xs) (x-1) )

insertAt x xs p = (take (p-1) xs) ++ x:[] ++ (lastPart xs p)

unique [] = []
unique xs = if elem (head xs) (tail xs) then unique (tail xs) else head xs: unique (tail xs)


sum' x = [y | z<-[1..x], y <- [1..z]]

index xs x = lastPart (take x xs) (x-1)

combinations 0 _ = []
combinations 1 xs =[x:[] | x<-xs]
combinations x xs=[z ++ zs | i<-[1..length' xs], let z = (index xs i), zs <-(combinations (x-1) (lastPart xs i))]

checkDiv x n
    | x == n = True
    | x `mod` n == 0 = False
    |otherwise = checkDiv x (n+1)

isPrime x = (checkDiv x 2)


