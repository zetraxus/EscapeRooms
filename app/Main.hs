module Main where

import System.IO (hFlush, stdout)
import Prelude

type GameState = (Int, [String], [[String]]) -- (nr pokoju, inventory, [p1, p2, p3...])

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

removeFromList :: Eq a => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList x (y:ys) | x == y = ys
                        | otherwise = y : removeFromList x ys

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs) | n == 0 = newVal:xs
                           | otherwise = x:replaceNth (n-1) newVal xs
                           
isOnList :: Eq a => a -> [a] -> Bool
isOnList x [] = False 
isOnList x (y:ys) | x == y = True
                  | otherwise = isOnList x ys

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

pickUp :: GameState -> String -> IO()
pickUp gameState object = do
  putStrLn "pickUp"

  let roomId = first(gameState)
      roomsState = third(gameState)
      currentRoomState = roomsState !! roomId

  if isOnList object currentRoomState then do --check if on the list
    let newCurrentRoomState = removeFromList object currentRoomState --remove object currentRoom
        newRoomsState = replaceNth roomId newCurrentRoomState roomsState --update roomsState
        newGameState = (roomId, second(gameState) ++ [object], newRoomsState) --update gameState
    printGameState newGameState
    game newGameState
    else do
      print "Przedmiotu nie ma w pokoju"
      printGameState gameState
      game gameState

printGameState :: GameState -> IO()
printGameState gameState = do
  let roomId = first(gameState)
      inventory = second(gameState)
      roomState = third(gameState) !! roomId
  print roomId
  print inventory
  print roomState
  putStrLn "=============="

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
  let cmd = head line
  case cmd of
    "rozejzyj" -> lookAround gameState
    "podnies" -> pickUp gameState (line !! 1)
    "czytaj" -> readNote
    "uzyj" -> use
    "przegladaj" -> showEq
    "wpisz" -> enterCode
    "ratunku!" -> help
    _ -> putStrLn "Bledna komenda"

initialGameState :: GameState
initialGameState = (0, [], [["klucz", "drzwi", "xd", "xd2"], ["klucz1", "drzwi1", "xd4", "xd3"]])

gameOver :: GameState -> Bool
gameOver gameState = first(gameState) == 1

game :: GameState -> IO()
game gameState = do
    if gameOver gameState then putStrLn "Koniec gry!"
      else do
        line <- getInputLine "Co robisz?";
        let tokenizedLine = tokenize line
        command tokenizedLine gameState

main :: IO ()
main = game initialGameState