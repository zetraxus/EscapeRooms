module Main where

import Lib
import System.IO (hFlush, stdout)
import Prelude

type GameState = (Int, [String], [[String]]) -- (nr pokoju, inventory, [p1, p2, p3...])

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

--remove :: Eq a => a -> [a] -> [a]
--remove :: String -> [String] -> [String]
removeFromList :: String -> [String] -> [String]
removeFromList x []                 = []
removeFromList x (y:ys) | x == y    = ys
                        | otherwise = y : removeFromList x ys


--remove :: Eq a => a -> [a] -> [a]
--remove element list = filter (\e -> e/=element) list

--replaceNth :: Int -> a -> [a] -> [a]
--replaceNth _ _ [] = []
--replaceNth n newVal (x:xs) | n == 0 = newVal:xs
--                           | otherwise = x:replaceNth (n-1) newVal xs

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
  let currentRoom = head(third(gameState))
--  let currentRoom = third(gameState) !! first(gameState) --get current room state
  let newCurrentRoom = removeFromList object currentRoom --remove object currentRoom
  putStrLn $ show newCurrentRoom
--  printList newCurrentRoom
--  (remove(object, currentRoom))

  --let newCurrentRoom = removeFromList(object, currentRoom)

--  let (ys,zs) = splitAt n xs in ys ++ (tail zs)
  --ys ++ zs
--  let xd = let (ys,zs) = splitAt first(gameState) third(gameState) in ys ++ ([newCurrentRoom] ++ tail zs)
  game (first(gameState), second(gameState) ++ [object], third(gameState))--replaceNth(first(gameState), newCurrentRoom, third(gameState)))--removeFromList(object, currentRoom))
--  game (first(gameState)+1, [object], third(gameState)) --TODO add check if object is avail

printList :: [String] -> IO()
printList list = putStrLn $ show list

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
    "podnosze" -> pickUp gameState (line !! 1)
    "czytam" -> readNote
    "uzywam" -> use
    "przegladam" -> showEq
    "wpisuje" -> enterCode
    "ratunku!" -> help
    _ -> putStrLn "Bledna komenda"

initialGameState :: GameState
initialGameState = (0, [], [["klucz", "drzwi", "xd", "xd2"], ["klucz1", "drzwi1", "xd4", "xd3"]])

gameOver :: GameState -> Bool
gameOver gameState = first(gameState) == 1

game :: GameState -> IO()
game gameState = do
--    printList (third(gameState) !! 0)
    if gameOver gameState then putStrLn "Koniec gry!"
      else do
        line <- getInputLine "Co robisz?";
        let tokenizedLine = tokenize line
        command tokenizedLine initialGameState


main = game initialGameState