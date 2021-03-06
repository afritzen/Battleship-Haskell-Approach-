{-- Represents a board for the game battleship. --}

module Board where

import Data.Array
import Data.Functor
import Data.List

data Ship = Active | Defeated | OpponentDefeated | OpponentActive | None deriving (Eq)

data Turn = Player | Computer deriving (Eq)

-- visualization of a ship, depending on it's state and owner
instance Show Ship where
	show Defeated = "x"
	show OpponentDefeated = "*"
	show OpponentActive = "#"
	show Active = ">"
	show None = " "

type Board = Array (Int, Int) Ship

type Coordinates = (Int, Int)
 
-- clearly separates the lines of a board when printing it 
delimiter2D :: String
delimiter2D = "\n-------------------------------------------\n"

-- board for testing
testBoard :: Board
testBoard = newBoard

-- creates a new board with 10x10 fields
newBoard :: Board
newBoard = array ((0, 0), (10, 10)) [((i, j), None) | i <- [0..10], j <- [0..10]]

-- prints the board
printBoard :: Board -> String
printBoard board = intercalate delimiter2D [intercalate " | " $ show <$> [board ! (j,i) | i <- [0..10]] | j <- [0..10]] ++ "\n"


