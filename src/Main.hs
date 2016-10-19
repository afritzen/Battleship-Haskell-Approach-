{-- Future main loop for the battleship game --}

module Main where

import Board
import Data.Array
import Text.Read

main :: IO ()
main = placeShipsPlayer testBoard 0

-- lets the player place all ships (3 total) on the board
placeShipsPlayer :: Board -> Int -> IO ()
placeShipsPlayer board counter  = do
				putStrLn "Welcome to battleship! \n"
				putStrLn $ printBoard $ board
				pos <- getCoordinates board
				let boardNew = board // [(pos, Active)]
				putStrLn $ printBoard $ boardNew
				if counter == 3 then
					do putStrLn "All ships placed."
					   placeShipsComputer boardNew 0	
				else
					placeShipsPlayer boardNew (counter + 1)

-- TODO
placeShipsComputer :: Board -> Int -> IO ()
placeShipsComputer board counter = do
					putStrLn "IMPLEMENT!"
-- TODO
startGame :: Board -> IO ()
startGame board = do 
		putStrLn "IMPLEMENT!"

-- reads the entered coordinates and returns them
getCoordinates :: Board -> IO Coordinates
getCoordinates board = do
			putStrLn "Where do you want to place your ship?"
			idx <- getLine
			coords <- case readMaybe idx :: Maybe Coordinates of
				Just p -> return p
				Nothing -> putStrLn "Invalid coordinates!" >> getCoordinates board 
			if not (inRange (bounds board) coords) then
				do putStrLn "Coordinates not in range!"
				   getCoordinates board
			else
				return coords

