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
isOnList _ [] = False 
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
  
printGameState :: GameState -> IO()
printGameState gameState = do
  let roomId = first(gameState)
      inventory = second(gameState)
      roomState = third(gameState) !! roomId
  print roomId
  print inventory
  print roomState
  putStrLn "=============="

lookAround :: GameState -> IO()
lookAround gameState = do
  printGameState gameState
  game gameState

pickUp :: GameState -> String -> IO()
pickUp gameState item = do
  let roomId = first(gameState)
      roomsState = third(gameState)
      currentRoomState = roomsState !! roomId

  if isOnList item currentRoomState then do --check if on the list
    let newCurrentRoomState = removeFromList item currentRoomState --remove item currentRoom
        newRoomsState = replaceNth roomId newCurrentRoomState roomsState --update roomsState
        newGameState = (roomId, second(gameState) ++ [item], newRoomsState) --update gameState
    printGameState newGameState
    game newGameState
  else do
    print "Przedmiotu nie ma w pokoju"
    printGameState gameState
    game gameState

readNote :: IO()
readNote = putStrLn "readNote"

use :: GameState -> String -> String -> IO()
use gameState item roomObject = do
  let roomId = first(gameState)
      inventoryState = second(gameState)
      roomsState = third(gameState)
      currentRoomState = roomsState !! roomId
  
  if roomId == 0 && item == "klucz" && roomObject == "drzwi" then do
    let newInventoryState = removeFromList item inventoryState
        newGameState = (roomId + 1, newInventoryState, roomsState)
    game newGameState
--  else if roomId == 0 && item == "xd" && roomObject == "xd2" then do
--    game gameState
  else do
    putStrLn "Nie mozna uzyc przedmiotu z tym obiektem"
    game gameState
  --czy mam item w inv
  --czy roomObj jest w pokoju
  -- (id, item, roomObj, )


showEq :: IO()
showEq = putStrLn "showEq"

enterCode :: IO()
enterCode = putStrLn "enterCode"

help :: GameState -> IO()
help gameState = do
  putStrLn "=============="
  putStrLn "Dostepne komendy:"
  putStrLn "rozejrzyj sie, podnies, czytaj, uzyj, przegladaj, wpisz kod"
  putStrLn "=============="
  game gameState

command :: [String] -> GameState -> IO ()
command line gameState = do
  let cmd = head line
  --TODO error check if line !! 3 exist
  case cmd of
    "rozejrzyj" -> lookAround gameState
    "podnies" -> pickUp gameState (line !! 1)
    "czytaj" -> readNote
    "uzyj" -> use gameState (line !! 1) (line !! 2)
    "przegladaj" -> showEq
    "wpisz" -> enterCode
    "pomocy" -> help gameState
    _ -> do putStrLn "Bledna komenda!"
            help gameState 
    

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