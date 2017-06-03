n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5] 

last' xs = xs !! ( length xs - 1 )

--- The opposite of equality
{- class Qe a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not ( x == y )
    y == y = not ( x /= y )

instance Qe Bool where
    False == True   = True
    True == True    = False
    _ == _          = False
-}

-- Show's 3 as a String
a = show 3
-- Convert a String into it's real type provided it's supported with an operator and another type
b = read "3" + 3
-- Or just needs support..
b' = read "5" :: Int 

-- Has type: twice :: (t -> t) -> t -> t using :t in GHCi
twice f x = f (f x)

signum :: Int -> Int
signum n = if n < 0 then -1 else
                if n == 0 then 0 else 1 

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
 | a > b = GT
 | a == b = EQ
 | otherwise = LT 


 -- Lambda;
 {-odds n = map f [0..n-1]
            where
                f x = x*2 + 1
-}
-- Simplies to (using lambda)...
odds :: Int -> [Int]
-- Map applies a function to every list item, input: x, output if x = 1: 1*2+1 = 3 for all 0 <= x < n-1
odds n = map (\x -> x*2 + 1) [0..n-1]

-- Using let to define functions, ... if type Vertex was defined...
{-triFun :: Vertex -> Vertex -> Vertex -> Float
triFun v1 v2 v3 =   let disbetween (x1, y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2) # must be on a single line? !
                    in distbetween v1 v2 +
                        distbetween v2 v3 +
                        distbetween v1 v3 
-}