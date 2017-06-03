module Reversi where

import Data.List

-- Position type and utility functions
type Position = (Int, Int)

-- ***
-- Given a Position value, determine whether or not it is a legal position on the board
isValidPos :: Position -> Bool
isValidPos (x,y) 	| x <= 7 && x >= 0 && y <= 7 && y >= 0 = True
					| otherwise = False


-- Player type and utility functions
data Player = PlayerWhite | PlayerBlack deriving (Eq)
instance Show Player where
	show PlayerWhite = "white"
	show PlayerBlack  = "black"

-- ***
-- Given a Player value, return the opponent player
otherPlayer :: Player -> Player
otherPlayer PlayerBlack = PlayerWhite
otherPlayer PlayerWhite = PlayerBlack

-- Piece type and utility functions
data Piece = Piece Position Player deriving (Eq)
instance Show Piece where
	show (Piece _ PlayerWhite) = " W"
	show (Piece _ PlayerBlack) = " B"

-- ***
-- Given a Player value and a Piece value, does this piece belong to the player?
isPlayer :: Player -> Piece -> Bool
isPlayer player (Piece _ pplayer ) 
					| player == pplayer = True
					| otherwise = False

{-

		GETTERS

-}

-- ***
-- Given a Piece value, determine who the piece belongs to
playerOf :: Piece -> Player
playerOf ( Piece _ player ) = player

-- ***
-- Given a piece, return its position
getPosition :: Piece -> Position
getPosition ( Piece pos _ ) = pos

-- ***
-- Flip a piece over
--flipPiece :: Piece -> Piece


-- Board type and utility functions
type Board = [Piece]

-- The initial configuration of the game board
initialBoard :: Board
initialBoard =
	[
		Piece (3,4) PlayerWhite, Piece (4,4) PlayerBlack,
		Piece (3,3) PlayerBlack, Piece (4,3) PlayerWhite
	]




-- ***
-- Given a Position value, is there a piece at that position?

isOccupied :: Position -> Board -> Bool
isOccupied pos [] = False
isOccupied pos (x:xs) 	| getPosition x == pos = True
						| otherwise = isOccupied pos xs
-- ***
-- Which piece is at a given position? 
-- Return Nothing in the case that there is no piece at the position
-- Otherwise return Just the_piece

pieceAt :: Position -> Board -> Maybe Piece
pieceAt pos [] = Nothing
pieceAt pos (x:xs)
				| getPosition x == pos = Just x
				| otherwise = pieceAt pos xs

-- ***
-- Determine if a particular piece can be placed on a board.  
-- There are two conditions: 
-- (1) no two pieces can occupy the same space, and 
-- (2) at least one of the other player's pieces must be flipped by the placement of the new piece.
validMove :: Piece -> Board -> Bool
validMove (Piece pos _) board
					| isOccupied pos board = False
					| otherwise = True


-- ***
-- Determine which pieces would be flipped by the placement of a new piece
--toFlip :: Piece -> Board -> [Piece]

-- ***
-- Auxillary function for toFlip. 
-- You don't have to use this function if you prefer to define toFlip some other way.
-- Determine which pieces might get flipped along a particular line 
-- when a new piece is placed on the board.  
-- The first argument is a vector (pair of integers) that describes 
-- the direction of the line to check.  
-- The second argument is the hypothetical new piece.  
-- The return value is either the empty list, 
-- a list where all pieces belong to the same player, 
-- or a list where the last piece belongs to the player of the hypothetical piece.  
-- Only in the last case can any of the pieces be flipped.

--getLineDir :: (Int, Int) -> Piece -> Board -> [Piece]

-- ***
-- Auxillary function for toFlip.
-- You don't have to use this function if you prefer to define toFlip some other way.
-- Given the output from getLineDir, determine which, if any, of the pieces would be flipped.

--flippable :: [Piece] -> [Piece]

-- ***
-- Place a new piece on the board.  Assumes that it constitutes a validMove
makeMove :: Piece -> Board -> Board
makeMove p b = p:b

-- ***
-- Find all valid moves for a particular player
allMoves :: Player -> Board -> [Piece]
allMoves p b = b ++ [Piece (1,1) PlayerWhite]

-- ***
-- Count the number of pieces belonging to a player
score :: Player -> Board -> Int
score p [] = 0
score p (x:xs) 	| show x == " B" && p == PlayerBlack = 1 + score p xs
				| show x == " W" && p == PlayerWhite = 1 + score p xs
				| otherwise = score p xs



-- ***
-- Decide whether or not the game is over. The game is over when neither player can make a validMove
isGameOver :: Board -> Bool
isGameOver b = False

-- ***
-- Find out who wins the game.  
-- Return Nothing in the case of a draw.
-- Otherwise return Just the_Player
winner :: Board -> Maybe Player
winner b = Nothing

