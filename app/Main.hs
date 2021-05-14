module Main where

import Lib
import System.IO (hFlush, stdout)
import Prelude

type GameState = (Int, [[String]])

getInputLine :: String -> IO String
getInputLine prompt = do
    putStrLn prompt
    hFlush stdout
    getLine

tokenize :: String -> [String]
tokenize command = do
  let splittedCommand = words command
  splittedCommand

lookAround :: GameState -> IO()
lookAround gameState = do
  putStrLn "lookAround"
  game gameState

pickUp :: GameState -> IO()
pickUp gameState = do
  putStrLn "pickUp"
  game (1, [["pusto", "pusto2"], ["po1", "po1a"]])--move to enter new room

readNote :: IO()
readNote = putStrLn "readNote"

use :: IO()
use = putStrLn "use"

showEq :: IO()
showEq = putStrLn "showEq"

enterCode :: IO()
enterCode = putStrLn "enterCode"

help :: IO()
help = putStrLn "help"

command :: [String] -> GameState -> IO ()
command line gameState = do
  let cmd = line !! 0
  case cmd of
    "rozgladam" -> lookAround gameState
    "podnosze" -> pickUp gameState
    "czytam" -> readNote
    "uzywam" -> use
    "przegladam" -> showEq
    "wpisuje" -> enterCode
    "ratunku!" -> help
    _ -> putStrLn "Bledna komenda"

initialGameState :: GameState
initialGameState = (0, [["pusto", "pusto2"], ["po1", "po1a"]])

gameOver :: GameState -> Bool
gameOver gameState = fst(gameState) == 1

game :: GameState -> IO()
game gameState = do
    if gameOver gameState then putStrLn "Koniec gry!"
      else do
        line <- getInputLine "Co robisz?";
        let tokenizedLine = tokenize line
        command tokenizedLine initialGameState


main = game initialGameState