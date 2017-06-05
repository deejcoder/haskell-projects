module GameTree where

import Data.List
import Data.Ord
import Reversi

-- Generic tree datatype
data Tree a = Root a [Tree a] deriving Show

getRoot :: Tree a -> a
getRoot (Root a _) = a

-- Function to build trees recursively.  f takes a tree element, and returns all
-- the child elements.
repTree :: (a -> [a]) -> a -> Tree a
repTree f root = Root root (map (repTree f) (f root))

-- Labels for the game tree.  We need to know the current board, and which was
-- the last piece to be played.
data Game = Game Piece Board deriving Show

getBoard :: Game -> Board
getBoard (Game _ board) = board

-- An adaptor function for allMoves.  It is slightly complicated by the
-- possibility that a player may be forced to skip a turn (if the player cannot
-- make any valid moves)
gameMoves :: Game -> [Game]
gameMoves (Game (Piece _ player) board) =
		map (\piece -> Game piece (makeMove piece board))
			(if (null moves) then movesPass else moves)
	where
		moves = allMoves (otherPlayer player) board
		movesPass = allMoves player board

-- Construct a game tree
gameTree :: Game -> Tree Game
gameTree root = repTree gameMoves root

-- Helper function to find out who's turn it is for any node in the game tree
movePlayer :: Tree Game -> Player
movePlayer (Root (Game (Piece _ player) _) _) = player

-- Estimate the value of a position for a player
-- This could be modified to account for strategic concerns, e.g. corners are
-- worth more than edges which are worth more than other positions.
estimate :: Player -> Game -> Int
estimate player (Game _ board) = 
	score player board
	+ estimateCount player board
	
--Using recursion is more efficient than list comprehension (with length, or summing) although messier
--Case 1: players don't correspond to player, continue to the next item
--Case 2: it's a corner, valued at score two
--Case 3: if it's not a corner, is it an edge as a corner is a subset of the edges
--Case 4: it must be a position in the middle
estimateCount :: Player -> Board -> Int
estimateCount player [] = 0
estimateCount player ((Piece pos player'):xs)
	| player /= player' = estimateCount player xs
	| isCorner pos = 2 + (estimateCount player xs)
	| isEdge pos = 1 + (estimateCount player xs)
	| otherwise = estimateCount player xs


{- using list comprehension
occupiedCorners player board = sum (
		[1 | (Piece pos p) <- board, (isCorner pos) && (p == player)]
	)
-}

isCorner :: Position -> Bool
isCorner pos
	| pos == (0,0) || pos == (7,7) || pos == (0,7) || pos == (7,0) = True
	| otherwise = False

isEdge :: Position -> Bool
isEdge (x,y)
	| x == 0 || x == 7 || y == 0 || y == 7 = True
	| otherwise = False 

{- ...when you forget you must also determine if it's the same player probably less efficient anyway
occupiedCorners :: Board -> Int
occupiedCorners board = 
	foldr (\corner -> case (isOccupied corner board) of
				True -> (+1)
				False -> (+0)
	) 0 [(0,7), (7,0), (7,7), (0,0)]

-}

-- Maximise or minimise the value of a game tree, depending on whose move it is
minimax :: Player -> Tree Game -> Int
minimax aiPlayer node =
	if movePlayer node == aiPlayer then maximise aiPlayer node
	else minimise aiPlayer node

-- Maximise the value of a game tree, return the maximised value
-- When we maximise, it is the ai player's turn
maximise :: Player -> Tree Game -> Int
maximise aiPlayer (Root x []) = estimate aiPlayer x
maximise aiPlayer (Root x subs) = maximum (map (minimax aiPlayer) subs)

-- Minimise the value of a game tree, return the minimised value
-- When we minimise, it is the other player's turn (not the ai player)
minimise :: Player -> Tree Game -> Int
minimise aiPlayer (Root x []) = estimate (otherPlayer aiPlayer) x
minimise aiPlayer (Root x subs) = minimum (map (minimax aiPlayer) subs)

-- Use maximise to find the best move
bestMove :: Player -> Tree Game -> Game
bestMove aiPlayer (Root _ subs) =
	getRoot (maximumBy (comparing (maximise aiPlayer)) subs)

-- Prune a game tree to a manageable number of levels
prune :: Int -> Tree Game -> Tree Game
prune 0 (Root x _) = Root x []
prune n (Root x sub)
	| n > 0 = Root x (map (prune (n - 1)) sub)

-- Determine the best move for a player
aiMove :: Int -> Player -> Game -> Game
aiMove lookahead player = (bestMove player).(prune lookahead).gameTree

