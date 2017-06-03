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
estimate player (Game _ board) = score player board

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

