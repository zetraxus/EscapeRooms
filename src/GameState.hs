module GameState
    (GameState, initialRoomsStates, initialGameState, getRoomId, getInventory, getRoomsStates, getCounters, getSequence,
     getItemsInCurrentRoom, printGameState, gameOver) where

--Stan gry: (room id, inventory, [room_1 items, room_2 items, ...], counters, sequence).
type GameState = (Int, [String], [[String]], [Int], [Int])

--Startowy stan przedmiotów w pokojach.
initialRoomsStates :: [[String]]
initialRoomsStates = [["klucz", "sztabka"], ["notatka", "list"], ["kartka"], ["drut", "blaszka"], ["piła", "siekiera", "manekin"]]

--Startowy stan gry
initialGameState :: GameState
initialGameState = (0, [], initialRoomsStates, [0, 2, 2, 0, 0], [0, 0, 0])

--Funkcja pozwalająca odczytać numer pokoju ze stanu gry.
--Parametry wejściowe: stan gry.
--Parametry wyjściowe: numer pokoju.
getRoomId :: GameState -> Int
getRoomId (a, _, _, _, _) = a

--Funkcja pozwalająca odczytać stan inventory ze stanu gry.
--Parametry wejściowe: stan gry.
--Parametry wyjściowe: stan inventory.
getInventory :: GameState -> [String]
getInventory (_, b, _, _, _) = b

--Funkcja pozwalająca odczytać stan przedmiotów w pokojach ze stanu gry.
--Parametry wejściowe: stan gry.
--Parametry wyjściowe: stan przedmiotów w pokojach.
getRoomsStates :: GameState -> [[String]]
getRoomsStates (_, _, c, _, _) = c

--Funkcja pozwalająca odczytać stan liczników ze stanu gry.
--Parametry wejściowe: stan gry.
--Parametry wyjściowe: liczniki.
getCounters :: GameState -> [Int]
getCounters (_, _, _, d, _) = d
  
--Funkcja pozwalająca odczytać stan sekwencji ze stanu gry.
--Parametry wejściowe: stan gry.
--Parametry wyjściowe: sekwencja.
getSequence :: GameState -> [Int]
getSequence (_, _, _, _, e) = e

--Funkcja pozwalająca odczytać przedmiotu w konkretnym pokoju ze stanu gry.
--Parametry wejściowe: stan gry.
--Parametry wyjściowe: lista przedmiotów w pokoju.
getItemsInCurrentRoom :: GameState -> [String]
getItemsInCurrentRoom gameState =
  getRoomsStates gameState !! getRoomId gameState

--Funkcja pozwalająca na wypisanie stanu gry.
--Parametry wejściowe: stan gry.
printGameState :: GameState -> IO()
printGameState gameState = do
  print $ getRoomId gameState
  print $ getInventory gameState
  print $ getItemsInCurrentRoom gameState
  print $ getCounters gameState
  print $ getSequence gameState

--Funkcja weryfikująca czy gra się skończyła.
--Parametry wejściowe: stan gry.
--Parametry wyjściowe: wartość logiczna opisująca czy gra się skończyła.
gameOver :: GameState -> Bool
gameOver gameState = getRoomId gameState == length (getRoomsStates gameState)