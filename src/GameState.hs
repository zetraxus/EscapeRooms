module GameState
    (GameState, initialRoomsStates, initialGameState, getRoomId, getInventory, getRoomsStates, getCounters, getSequence,
     getItemsInCurrentRoom, printGameState, gameOver) where

-- (room id, inventory, [room_1 items, room_2 items, ...], counters, sequence)
type GameState = (Int, [String], [[String]], [Int], [Int])

initialRoomsStates :: [[String]]
initialRoomsStates = [["klucz", "sztabka"], ["notatka", "list"], ["kartka"], ["drut", "blaszka"], ["piła", "siekiera", "manekin"]]

initialGameState :: GameState
initialGameState = (0, [], initialRoomsStates, [0, 2, 2, 0, 0], [0, 0, 0])

getRoomId :: GameState -> Int
getRoomId (a, _, _, _, _) = a

getInventory :: GameState -> [String]
getInventory (_, b, _, _, _) = b

getRoomsStates :: GameState -> [[String]]
getRoomsStates (_, _, c, _, _) = c

getCounters :: GameState -> [Int]
getCounters (_, _, _, d, _) = d
  
getSequence :: GameState -> [Int]
getSequence (_, _, _, _, e) = e

getItemsInCurrentRoom :: GameState -> [String]
getItemsInCurrentRoom gameState =
  getRoomsStates gameState !! getRoomId gameState

printGameState :: GameState -> IO()
printGameState gameState = do
  print $ getRoomId gameState
  print $ getInventory gameState
  print $ getItemsInCurrentRoom gameState
  print $ getCounters gameState
  print $ getSequence gameState

gameOver :: GameState -> Bool
gameOver gameState = getRoomId gameState == length (getRoomsStates gameState)