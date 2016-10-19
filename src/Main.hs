{-- Future main loop for the battleship game --}

module Main where

import Board
import Data.Array
import Text.Read
import System.Random

main :: IO ()
main = do 
	putStrLn "WELCOME TO BATTLESHIP!\n"
	placeShipsPlayer testBoard 0

-- lets the player place all ships (4 total) on the board
placeShipsPlayer :: Board -> Int -> IO ()
placeShipsPlayer board counter  = do
				putStrLn $ printBoard $ board
				pos <- getCoordinates board
				if alreadyOccupied board pos then
					putStrLn "This field already holds a ship!\n" >> placeShipsPlayer board counter
				else
					do let boardNew = board // [(pos, Active)]
					   putStrLn $ printBoard $ boardNew
					   if counter == 3 then
					   	do putStrLn "All ships placed.\n"
					           placeShipsComputer boardNew 0	
					    else
						   placeShipsPlayer boardNew (counter + 1)
				

-- lets the computer randomly place all ships (4 total) on the board 
placeShipsComputer :: Board -> Int -> IO ()
placeShipsComputer board counter = do
					rnd <- randomRIO (0, 10 :: Int)
					rnd2 <- randomRIO (0, 10 :: Int)
					let pos = (rnd, rnd2)
					if alreadyOccupied board pos then
						placeShipsComputer board counter
					else
						do let boardNew = board // [(pos, OpponentActive)]
					           putStrLn $ printBoard $ boardNew
						   if counter == 3 then
							do putStrLn "Computer placed all ships.\n"
							   startGame boardNew
						   else
							placeShipsComputer boardNew (counter+ 1)	
								
			
-- TODO
startGame :: Board -> IO ()
startGame board = do 
		putStrLn "First phase completed! Game starting ...\n"
		putStrLn $ printBoard board


{-- ## Helper functions go here ## --}

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

-- determines whether a field on the board already holds a ship
alreadyOccupied :: Board -> (Int, Int) -> Bool
alreadyOccupied board coords | board !coords == None = False
			     | otherwise = True	 





