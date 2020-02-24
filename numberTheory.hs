


isPrime :: Int -> Bool
isPrime 2 = True
isPrime n | (n `mod` 2 == 0) = False
          | otherwise        = foldl iterator True [3,5..upper]
        where upper    = floor . sqrt . fromIntegral $ n
              iterator = \acc x -> if n `mod` x == 0 then False else acc

primesUpTo :: Int -> [Int]
primesUpTo n
           | n<=0      = error "cannot give list of primes up to non-positive value"
           | otherwise = filter isPrime [2..n]

--jacobiSymbol :: Int -> Int -> Int
--jacobiSymbol a n
--    |
--    |
--    |

dotProduct2 :: (Num a) => (a,a) -> (a,a) -> a
dotProduct2 (a1, a2) (a3, a4) = a1*a3+a2*a4

chineseRemainder :: [(Int, Int)] -> (Int, Int)
-- example x === 3 mod 5
--         x === 2 mod 9   =>  chineseRemainder [(3,5), (2,9), (7,23)] = ()
--         x === 7 mod 23
chineseRemainder [] = error "empty input to chineseRemainder"
chineseRemainder ((a1, n1):[]) = (a1, n1)
chineseRemainder ((a1, n1):(a2, n2):rest)
    | g>1 &&  ((mod a1 g) == (mod a2 g)) = chineseRemainder ((a3 `div` g, n3):rest)
    | g>1                               = (negate 1, negate 1)
    | otherwise                         = chineseRemainder ((a3, n3):rest) -- coprime
    where g   = gcd n1 n2
          n1' = modInv n1 n2
          n3  =n1*n2
          a3  = mod (a2*(n1'*n1)+ a1*(1-n1'*n1)) n3 --dotProduct2 (head (gcdValues n1 n2)) (a1, a2)



gcd' :: Int -> Int -> Int
-- finding the gcd amounts to running the euclidean algorithm,
-- which is based on the residuals r_(k-2)=q_k r_(k-1)+r_k
-- for each step r_k-1 and r_k-2 are our nonnegative remainders, initally a and b
-- the remainders decrease for eqch step, and we eventually get r_N=0, from here
-- we recursively compute
-- a=q_2 b   + r_2
-- b=q_3 r_2 + r_3
-- ...
-- r_(N-4)=q_(N-2) r_(N-3)+r_(N-2)       =      q_(N-2) (q_(N-1) q_N r_(N-1)+r_(N-1))+r_(N-2)
-- r_(N-3)=q_(N-1) r_(N-2)+r_(N-1)       =      q_(N-1) q_N r_(N-1)+r_(N-1)
-- r_(N-2)=q_N r_(N-1)+r_N               =      q_N r_(N-1)                 r_(N-1) is gcd! (see above, r_(N-1) common factore in all)
-- correct!
gcd' a b
        | (a `mod` b == 0) = b
        | (b `mod` a == 0) = a
        | a < b            = gcd' b a
        | otherwise        = gcd' b r
        where r = a `mod` b

gcdValues :: Int -> Int -> [(Int,Int)]
-- do gcd and record all      qs  & rs
-- works
gcdValues a b
        | (a `mod` b == 0) = [(0,b)]
        | a < b            = gcdValues b a
        | otherwise        = (q,r):gcdValues b r
        where q=a `div` b
              r=a `mod` b

modInv :: Int -> Int -> Int
modInv a n
           | a>n       = modInv (a `mod` n) n
           | a<0       = modInv (n - ((negate a) `mod` n)) n
           | otherwise = snd ( foldr1 iterator qr )
           where qr        = gcdValues a n
                 iterator  = \(q,r) (acc_a, acc_b) -> (acc_b, acc_a -q*acc_b+n) --- +n to get in 0< <n value
                 --foldStart = (0::Int,1::Int)
-- [ a*(a `modInv` n) `mod` n | n<-primesUpTo 1000, a<-[1..n-1]] is all ones, beibi!

modInvBoth :: Int -> Int -> (Int, Int)
modInvBoth a n
           | a>n       = modInvBoth (a `mod` n) n
           | a<0       = modInvBoth (n - ((negate a) `mod` n)) n
           | otherwise = foldr1 iterator qr
           where qr        = gcdValues a n
                 iterator  = \(q,r) (acc_a, acc_b) -> (acc_b, acc_a -q*acc_b+n) --- +n to get in 0< <n value
                 --foldStart = (0::Int,1::Int)
-- [ a*(a `modInv` n) `mod` n | n<-primesUpTo 1000, a<-[1..n-1]] is all ones, beibi!


toBinary :: Int -> [Int]
toBinary 1 = [1]
toBinary 0 = [0]
toBinary n = ((mod n 2) : toBinary (div n 2))

toNary :: Int -> Int -> [Int]
toNary m n
           | m < 0     = toNary (negate m) n
           | m < n     = [m]
           | otherwise = ((mod m n) : toNary (div m n) n)

toDecimal :: [Int] -> Int -> Int
toDecimal _ 0                          = error "base 0 not possible"
toDecimal [number] base                = number
toDecimal (numberHead:numberTail) base = numberHead + base * (toDecimal numberTail base)
