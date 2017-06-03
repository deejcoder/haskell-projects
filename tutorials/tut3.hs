insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs)
	| a > x = x : insert a xs
	| otherwise = a : x : xs
	
isort :: [Int] -> [Int]
isort[] = []
isort (x:xs) =
	isort smaller ++ [x] ++ isort larger
	where 
		smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b >= x]
	
deleteAt :: Int -> [a] -> [a]
deleteAt 0 (x:xs) = xs
deleteAt n (x:xs) | n >= 0 = x : (deleteAt (n-1) xs)
deleteAt _ _ = error "index out of range"

duplicated' :: Eq a => a -> [a] -> Bool	
duplicated' a (x:xs) =
	if a == x
	then a `elem` ( drop ( elemIndex a xs ) xs )
	else False
