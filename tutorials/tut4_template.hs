data Tree a  = Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Ord, Show)

instance Functor Tree where
   fmap f (Leaf x) = Leaf (f x)
   fmap f (Branch leftsub rightsub) = Branch (fmap f leftsub) (fmap f rightsub)

foldTree :: (a -> a -> a) -> Tree a -> a
foldTree op (Leaf x) = x
foldTree op (Branch t1 t2) = (foldTree op t1) `op` (foldTree op t2)

data Suit = Clubs | Diamonds | Hearts | Spades 
     deriving (Eq, Ord, Show, Enum)

data CardValue = Ace | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 | Jack | Queen | King
     deriving (Eq, Enum, Ord)

instance Show CardValue where
   show Ace = "Ace"
   show V2 = "2"
   show V3 = "3"
   show V4 = "4"
   show V5 = "5"
   show V6 = "6"
   show V7 = "7"
   show V8 = "8"
   show V9 = "9"
   show V10 = "10"
   show Jack = "Jack"
   show Queen = "Queen"
   show King = "King"
   
data Card = Card CardValue Suit
     deriving (Eq, Ord)
     
value :: Card -> CardValue
value (Card v _) = v

suit :: Card -> Suit
suit (Card _ s) = s

instance Show Card where
  show (Card v s) = show v ++ " of " ++ show s
  
type Hand = [Card]

data Trumps = Club | Diamond | Heart | Spade | NoTrump
     deriving (Eq, Ord, Show, Enum)
   
data Bid = Pass | Bid Int Trumps deriving (Eq, Ord)

instance Show Bid where
  show Pass = "Pass"
  show (Bid i t) = show i ++ " " ++ show t
  
 
square :: Int -> Int
square b = b*b

-- Question 1A
applyEach :: [a -> b] -> a -> [b]
applyEach [] _ = []
applyEach (f:fs) a = f a : applyEach fs a


-- Question 1B
allOverZero :: [Int] -> Bool
allOverZero [] = True
allOverZero (x:xs)
				| x > 0 = True && allOverZero xs
				| otherwise = False
				

				
-- Question 2A
fringe :: Tree a -> [a]
fringe t = foldTree (++) (fmap (\x -> [x]) t) 

-- Question 2B
treeSize :: Tree a -> Int
treeSize t = foldTree (+) (fmap (\_ -> 1) t )

-- Question 2C
treeHeight :: Tree a -> Int
treeHeight ( Branch l r ) = foldTree(+) ( fmap ( \_ -> 0 ) l ) + 1 + foldTree(+) ( fmap ( \_ -> 0 ) r )


