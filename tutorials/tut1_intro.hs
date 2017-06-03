split :: [a] -> ([a],[a])

split xs = ( take (length xs `div` 2) xs, drop (length xs `div` 2) xs )

--contained at start only?
beginsWith :: String -> String -> Bool
beginsWith [] ys = True
beginsWith (x:xs) [] = False
beginsWith (x:xs) (y:ys) = (x == y) && beginsWith xs ys

-- Finds a string in another string
find :: String -> String -> Bool
find (x:xs) [] = False
find xs ys
    | beginsWith xs ys = True
    | find xs (tail ys) = True
    | otherwise = False

{-split' :: String -> String -> (String, String)
split' -}

{-

        BASICS
-}
middle :: [a] -> [a]
middle xs = init( tail xs )

heads :: [Int] -> [Int] -> [Int] -> [Int]
heads xs ys zs = [head xs, head ys, head zs]


product' :: Num a => [a] -> a
product' (x:xs) = x*(product xs)

joinLists :: [[a]] -> [a]
joinLists [] = []
joinLists (x:xs) = x ++ (joinLists xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

evenlength :: [a] -> Bool
evenlength xs = if (length xs) `mod` 2 == 0
                    then True
                else
                    False

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs =  if evenlength xs
                then split xs
            else
                ([], xs)

halve' xs
    |   xs == [] = ([], [])
    |   evenlength xs = split xs
    |   otherwise = ([], xs)


-- x:[] -> adds x to empty list, head xs -> 2nd value in list
--- base cases: empty list return empty, only one item in list return it self
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
-- If below returns one item in list, the above is executed, and exits
compress (x:xs) =   if x == (head xs)
                        then compress xs
                    else
                        x:compress xs

--          LIST COMPREHENSION

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]


positions :: Eq a => a -> [a] -> [Int]
positions value xs =
    -- zip returns (value in xs, index(given by [0..n)),
    -- where x (the zipped value) is the same as input 'value'
    [index | (x, index) <- zip xs [0..n], value == x]
    where n = length xs - 1 

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0, x /= n]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]

{-
        RECURSION

-}

qsort :: [Int] -> [Int]
-- Base case: an empty list is already sorted.
qsort [] = []
{-  
    - Using the head, find all smaller values, and larger values
    - Then for every value in smaller, determine the smallers of smaller,...
    - When at the last value, append the last value to the second to last's head,
        repeat appending until all instances have been appended (all values).
-}
qsort (x:xs) =
    qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x] 

{-

        HIGHER ORDER FUNCTIONS

-}
{-
    Structure of foldr;
        foldr :: (a -> b -> b) -> b -> [a] -> b
        foldr f v [] = v
        foldr f v (x:xs) = f x (foldr f v xs)

    Original structure of reverse;
    reverse' (x:xs) = (reverse' xs) ++ [x]

-}
reverse'' :: [a] -> [a]
-- \x xs -> xs ++ [x]: get the current list, add [x] to it. \x: input list. xs is defined by xs ++ [x]
reverse'' =
    foldr (\x xs -> xs ++ [x]) [] -- takes xs as a function it seems

{- i.e
    foldr f v (x:xs) = {f =} \x xs -> xs ++ [x] x {<-input} (foldr f v xs)


-}


strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters" 

type Position = (Int, Int)
data Player = PlayerWhite | PlayerBlack deriving (Eq)
instance Show Player where
	show PlayerWhite = "white"
	show PlayerBlack  = "black"
data Piece = Piece Position Player deriving (Eq)
class IsPos Piece where
    (==) :: 
    (Piece pos1 _ ) == (Piece pos2 _ ) = pos1 == pos2

type Board = [Piece]

initialBoard :: Board
initialBoard =
	[
		Piece (3,4) PlayerWhite, Piece (4,4) PlayerBlack,
		Piece (3,3) PlayerBlack, Piece (4,3) PlayerWhite
	]

printp :: Piece -> Piece -> Bool
printp p1 p2 = p1 IsPos p2

