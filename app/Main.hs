module Main where

import System.IO (hFlush, stdout)
import System.Exit
import Prelude

type GameState = (Int, [String], [[String]], [Int]) -- (nr pokoju, inventory, [p1, p2, p3...], counters)
type WorldDescription = [String]

initialGameState :: GameState
initialGameState = (1, [], [["klucz"], ["notatka", "list"], ["xd"]], [0, 2]) --TODO change inital state to 0

worldDescription :: WorldDescription
worldDescription = ["Ocknales sie. Lezysz na podlodze w dziwnym pomieszczeniu. Pierwszy raz je widzisz. Wstajesz i przecierasz oczy. To nie jest sen. Na scianie przed Toba widnieje namazany czerwona substacja napis: 'Nie ma ratunku!'. W pokoju znajduje sie jeszce stolik oraz wielkie czerwone drzwi. Podchodzisz... Na stole lezy klucz.",
  "Przechodzisz do drugiego pokoju. Na podlodze lezy szkielet. Chyba jest to szkielet Twojego poprzednika, ktoremu nie udalo sie uciec. Szkielet trzyma w rece jakies zawiniatko - chyba jest to jakis list. Dodatkowo widzisz jeszcze stolik, na ktorym lezy notatka. Kolejne drzwi sa zamkniete jednak zamiast tradycyjnego klucza potrzebujesz wpisac kod.",
  "Wchodzisz do kolejnego pokoju. Po Twojej lewej stronie stoi regal pelen ksiazek, zas z prawej widzisz trzy kolorowe dzwignie - niebieska, zielona i czerwona. Pod regalem znajduje sie sterta brudnych ubran, a posrod nich mozna rowniez zobaczyc dlugopis bez skuwki, zgnieciony papier, paczke zapalek i kilka drobnych monet.",
  "Jestes w 4 pokoju."]

first :: (a, b, c, d) -> a
first (a, _, _, _) = a

second :: (a, b, c, d) -> b
second (_, b, _, _) = b

third :: (a, b, c, d) -> c
third (_, _, c, _) = c

fourth :: (a, b, c, d) -> d
fourth (_, _, _, d) = d

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
      counters = fourth(gameState)
  print roomId
  print inventory
  print roomState
  print counters
  putStrLn "=============="

lookAround :: GameState -> IO()
lookAround gameState = do
  --printGameState gameState
  let roomId = first(gameState)
      description = worldDescription
  putStrLn (description !! roomId)
  game gameState

pickUp :: GameState -> String -> IO()
pickUp gameState item = do
  let roomId = first(gameState)
      inventoryState = second(gameState)
      roomsState = third(gameState)
      counters = fourth(gameState)
      currentRoomState = roomsState !! roomId

  if isOnList item currentRoomState then do --check if on the list
    let newCurrentRoomState = removeFromList item currentRoomState --remove item currentRoom
        newRoomsState = replaceNth roomId newCurrentRoomState roomsState --update roomsState
        newGameState = (roomId, inventoryState ++ [item], newRoomsState, counters) --update gameState
    game newGameState
  else do
    print "Przedmiotu nie ma w pokoju"
    game gameState

readNote :: GameState -> String -> IO()
readNote gameState item = do
  let inventoryState = second(gameState)
  if isOnList item inventoryState then do
    if item == "list" then do
      putStrLn "Witaj podrozniku!\nJesli czytasz ten list prawdopodobnie jestes w takiej samej sytuacji jak ja teraz. Notatka jest zdradliwa, aby otworzyc drzwi musisz podac prawidlowy kod, jednak notatka nie precyzuje dokladnie jaki jest kod. Niestety po 3 probie wpisanie kodu mechanizm zablokowal sie, a na wyswietlaczu pojawil sie licznik. Po kilku dniach zorientowalem sie, ze licznik pokazuje liczbe dni do jakiegos wydarzenia. (Byc moze do oblokowania mechanizmu) Zostaly mi jeszcze 83 dni. Jesli to czytasz chcialbym Cie poinformowac, ze poczatkowe liczby pierwsze, ktore wyznaczylem do obliczenia kodu to 11, 13 i 29. Niestety zadna z nich nie jest prawidlowa.\nSprawdz inne liczby. Moze Ci sie uda.\nPowodzenia!"
      game gameState
    else if item == "notatka" then do
      putStrLn "Wez liczbe pierwsza pomiedzy 10 a 30, podnies ja do kwadratu, a nastepnie pomnoz przez liczbe pelnych tygodni w kazdym roku."
      game gameState
    else do
      putStrLn "Nie mozesz przeczytac tego przedmiotu"
      game gameState
  else do
    putStrLn "Nie posiadasz przedmiotu o takiej nazwie"
    game gameState
  

use :: GameState -> String -> String -> IO()
use gameState item roomObject = do
  let roomId = first(gameState)
      inventoryState = second(gameState)
      roomsState = third(gameState)
      counters = fourth(gameState)
      currentRoomState = roomsState !! roomId
  if isOnList item inventoryState then do
    if roomId == 0 && item == "klucz" && roomObject == "drzwi" then do
      let newInventoryState = removeFromList item inventoryState
          newGameState = (roomId + 1, newInventoryState, roomsState, counters)
      lookAround newGameState
--    else if roomId == 1 && item == "xd" && roomObject == "xd2" then do
--      game gameState
    else do
      putStrLn "Nie mozna uzyc przedmiotu z tym obiektem"
      game gameState
  else do
    putStrLn "Nie posiadasz przedmiotu o takiej nazwie"
    game gameState

showEq :: GameState -> IO()
showEq gameState = do
  let inventoryState = second(gameState)
  print inventoryState
  game gameState

enterCode :: GameState -> String -> IO()
enterCode gameState code = do
  let roomId = first(gameState)
      inventoryState = second(gameState)
      roomsState = third(gameState)
      counters = fourth(gameState)
      triesLeftCode = counters !! roomId
  if code == "27508" then do
    putStrLn "bzzz: RAWIDLOWY KOD" 
    let newGameState = (roomId + 1, inventoryState, roomsState, counters)
    lookAround newGameState
  else if triesLeftCode == 0 then do 
    putStrLn "bzzz: ZLY KOD"
    putStrLn "52"
    putStrLn "Koniec gry: Umarles z glodu!"
  else do
    let newTriesLeftCode = triesLeftCode - 1
        newCounters = replaceNth roomId newTriesLeftCode counters --update counters
        newGameState = (roomId, inventoryState, roomsState, newCounters) --update gameState
    putStrLn "bzzz: ZLY KOD"
    game newGameState

help :: GameState -> IO()
help gameState = do
  putStrLn "=============="
  putStrLn "Dostepne komendy:"
  putStrLn "rozejrzyj sie, podnies, przeczytaj, uzyj, przegladaj ekwipunek, wpisz kod, koniec"
  putStrLn "=============="
  game gameState

wrongCommand :: GameState -> IO()
wrongCommand gameState = do
  putStrLn "Bledna komenda!"
  help gameState

command :: [String] -> GameState -> IO ()
command line gameState = do
  if (length line) > 3 then wrongCommand gameState
  else do
    let cmd = head line

    case cmd of
      "rozejrzyj" -> lookAround gameState
      "podnies" -> pickUp gameState (line !! 1)
      "przeczytaj" -> readNote gameState (line !! 1)
      "uzyj" -> use gameState (line !! 1) (line !! 2)
      "przegladaj" -> showEq gameState
      "wpisz" -> if (length line) == 3 && (line !! 1) == "kod" then do enterCode gameState (line !! 2)
                 else wrongCommand gameState
      "pomocy" -> help gameState
      "koniec" -> exitSuccess
      _ -> do wrongCommand gameState

gameOver :: GameState -> Bool
gameOver gameState = first(gameState) == 3

game :: GameState -> IO()
game gameState = do
    if gameOver gameState then putStrLn "Koniec gry!"
    else do
      line <- getInputLine "Co robisz?";
      let tokenizedLine = tokenize line
      command tokenizedLine gameState

main :: IO ()
main = lookAround initialGameState