module Main where

import Lib
import System.IO (hFlush, stdout)
import Prelude

type GameState = [Int]

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
  game [0,0,0]

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
initialGameState = [1, 1, 1]

gameOver :: GameState -> Bool
gameOver gameState = gameState == [0, 0, 0]

game :: GameState -> IO()
game gameState = do
    if gameOver gameState then putStrLn "Koniec gry!"
      else do
        line <- getInputLine "Co robisz?";
        let tokenizedLine = tokenize line
        command tokenizedLine initialGameState


main = game initialGameState