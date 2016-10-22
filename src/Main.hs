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
				putStrLn "Player can now place ships.\n"
				putStrLn "Where do you want to place your ship?\n"
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
							   putStrLn "First phase completed! Game starting ...\n"
							   startGame boardNew 4 4 Player
						   else
							placeShipsComputer boardNew (counter+ 1)	
								
			
-- TODO
startGame :: Board -> Int -> Int -> Turn -> IO ()
startGame board myShips oppShips turn = do
						if turn == Player then
							do putStrLn "Please take a guess:\n"
							   playerGuess <- getCoordinates board
							   putStrLn $ show playerGuess
						else
							do putStrLn "Computer takes a guess ...\n"
	  						   startGame testBoard myShips oppShips $ switchTurn turn	 
					

{-- ## Helper functions go here ## --}

-- reads the entered coordinates and returns them
getCoordinates :: Board -> IO Coordinates
getCoordinates board = do
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

-- determines wheter guessed coordinates hold one of the opponent's ships
hasOpponentShip :: (Int, Int) -> Board -> Bool
hasOpponentShip coords board | board !coords == OpponentActive = True
			     | otherwise = False 

-- determines wheter guessed coordinates hold one of the player's ships
hasPlayerShip :: (Int, Int) -> Board -> Bool
hasPlayerShip coords board | board !coords == Active = True
			   | otherwise = False

-- switches turn to either player or computer
switchTurn :: Turn -> Turn
switchTurn turn | turn == Player = Computer
		| otherwise = Player






