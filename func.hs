
-- Function definitions
evenlength::[a] -> Bool
split'::[a] -> ( [a],[a] )
halve::[a] -> ( [a], [a] ) 
halve':: [a] -> ( [a], [a] )
allbuttwo::[a] -> [a]
allbuttwo'::[a] -> [a]
--removeMinMax::[a] -> [a]
middleNumber::Int->Int->Int->Int
maxThreeOccurs::Int->Int->Int->(Int,Int)


--Question 1
----------------------------------------------------------

-- Determines whether or not a list is of even length.
evenlength xs = if (length xs) `mod` 2 == 0
				then True
				else False
				
--	Belongs to halve, performs the split.
split' xs = ( take ( length xs `div` 2 ) xs,
				drop ( length xs `div` 2 ) xs )

-- Halves a list into two if it's even.
halve xs = if null xs
				then ( [], [] )
				else if evenlength xs
				then split' xs
				else ( [], xs )
				
-- halve' a guarded equation version of halve.
--	Where all conditional branches must return the same types ([],[])
halve' xs | null xs = ( [], [] )
			| evenlength xs = ( split' xs )
			| otherwise = ( [], xs )
			
			
--Question 2
----------------------------------------------------------

allbuttwo xs = if ( length xs ) > 1
				then take ( length xs -2 ) xs
				else []
				
allbuttwo' xs | length xs > 1 = take ( length xs - 2 ) xs
				| otherwise = []
				
--Question 3
----------------------------------------------------------

-- Using lambda for all x in xs, x cannot be minimum or maximum
removeMinMax' xs = filter ( \x -> x /= minimum xs && x /= maximum xs ) xs

middleNumber a b c = head ( removeMinMax' nums )
					where nums = [a,b,c]
					
--Question 4 ------ what's being asked of here?
--How many times the element is in the input (occurs)?
--How many times the max is applied for each element?
----------------------------------------------------------
-- Assuming occurances: if only Haskell allowed for infix of equal types (i.e a == b == c)
-- a == b && b == c -> a == b == c -> all three equal then first element, max occurances
-- a is b or c, and a is of maxima then two elements are the same
-- b is c, and is maxima then two occurances.
-- else no elements are equal, occurance = 1

maxThreeOccurs a b c | a == b && b == c = (a, 3)
					| ( a == b || a == c ) && a == maxima = ( a, 2 )
					| b == c && b == maxima = ( b, 2 )
					| otherwise = ( maxima, 1 )
					where maxima = max ( max a b ) c