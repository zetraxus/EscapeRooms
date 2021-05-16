module Main where

import System.IO (hFlush, stdout)
import System.Exit
import Prelude

type GameState = (Int, [String], [[String]], [Int]) -- (nr pokoju, inventory, [p1, p2, p3...], counters)

initialGameState :: GameState
initialGameState = (3, [], [["klucz"], ["notatka", "list"], ["papier", "worek"], ["drut", "blaszka"], ["pila", "siekiera", "papier-scierny", "cialo"], ["xd"]], [0, 2, 0, 0, 0, 0]) --TODO change inital state to 0

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
  let roomId = first gameState
      inventory = second gameState
      roomState = third gameState !! roomId
      counters = fourth gameState
  print roomId
  print inventory
  print roomState
  print counters
  putStrLn "=============="

lookAround :: GameState -> IO()
lookAround gameState = do
  --printGameState gameState
  let roomId = first gameState
  case roomId of
    0 -> lookAround0 gameState
    1 -> lookAround1 gameState
    2 -> lookAround2 gameState
    3 -> lookAround3 gameState
    4 -> lookAround4 gameState
    5 -> lookAround5 gameState
    _ -> do putStrLn "Pokoj nie istnieje"

lookAround0 :: GameState -> IO()
lookAround0 gameState = do
  let roomId = first gameState
      roomState = third gameState !! roomId
      outputPart1 = "Ocknales sie. Lezysz na podlodze w dziwnym pomieszczeniu. Pierwszy raz je widzisz.\nWstajesz i przecierasz oczy. To nie jest sen. Na scianie przed Toba widnieje\nnamazany czerwona substacja napis: 'Nie ma ratunku!'. W pokoju znajduje sie jeszcze\nstolik oraz wielkie czerwone drzwi."
      outputPart2 = " Podchodzisz... Na stole lezy klucz."
  if isOnList "klucz" roomState then do
    let outputModified = outputPart1 ++ outputPart2
    putStrLn outputModified
  else do
    putStrLn outputPart1
  game gameState

lookAround1 :: GameState -> IO()
lookAround1 gameState = do
  let roomId = first gameState
      roomState = third gameState !! roomId
      outputPart1 = "Przechodzisz do drugiego pokoju. Na podlodze lezy szkielet. Chyba jest to szkielet\nTwojego poprzednika, ktoremu nie udalo sie uciec."
      outputPart2 = " Szkielet trzyma w rece jakies\nzawiniatko - chyba jest to jakis list."
      outputPart3 = " Dodatkowo widzisz jeszcze stolik, na ktorym\nlezy notatka."
      outputPart4 = " Kolejne drzwi sa zamkniete jednak zamiast tradycyjnego klucza potrzebujesz wpisac kod."
  if isOnList "list" roomState && isOnList "notatka" roomState then do
    let outputModified = outputPart1 ++ outputPart2 ++ outputPart3 ++ outputPart4
    putStrLn outputModified
  else if isOnList "notatka" roomState then do
    let outputModified = outputPart1 ++ outputPart3 ++ outputPart4
    putStrLn outputModified
  else if isOnList "list" roomState then do
    let outputModified = outputPart1 ++ outputPart2 ++ outputPart4
    putStrLn outputModified
  else do
    let outputModified = outputPart1 ++ outputPart4
    putStrLn outputModified
  game gameState

lookAround2 :: GameState -> IO() --TODO not modular not implemented
lookAround2 gameState = do
  let output = "Wchodzisz do kolejnego pokoju. Po Twojej lewej stronie stoi regal pelen ksiazek,\nzas z prawej widzisz dziwny panel z trzema otworami. Pod regalem znajduje sie\nsterta brudnych ubran, a posrod nich mozna rowniez zobaczyc dlugopis bez skuwki,\nzgnieciony papier, paczke zapalek i kilka drobnych monet. W rogu pokoju stoi worek\nz trzema kolorowymi przekladniami - niebieska, zielona i czerwona."
  putStrLn output
  game gameState

lookAround3 :: GameState -> IO() --TODO not modular not implemented
lookAround3 gameState = do
  let output = "W nastepnym pomieszczeniu nic nie widac. Wszedzie unosi sie dym. Jednak nie masz wyjscia,\nwchodzisz do pomieszczenia zaslaniajac usta i nos rekami. Drzwi zatrzaskuja sie za Toba.\nPo omacku badasz pomieszczenie. Na podlodze lezy kawalek drutu, blaszka, butelka z woda\ni recznik. Moczysz recznik woda i tworzysz z niego cos w rodzaju maski. Teraz mozesz\neksplorowac dalej. Znajdujesz drzwi. Nie ma zamka, zamiast niego natrafiasz na stara klodke."
  putStrLn output
  game gameState

lookAround4 :: GameState -> IO() --TODO not modular not implemented
lookAround4 gameState = do
  let output = "Po odblokowaniu drzwi pedzisz dalej co sil w nogach, by tylko nabrac do pluc swiezego\npowietrza. Niestety, gdy opuszczasz kleby dymu, do Twoich nozdrzy dociera okropny\nsmrod. Dookola panuje mrok. Nie jestes w stanie wytrzymac odoru i zwracasz swoje\nsniadanie wprost przed siebie. Po chwili decydujesz sie przejsc po omacku dalej,\nale Twoja noga trafia na cos miekkiego. Gdy twoj wzrok przyzwyczaja sie do ciemnosci,\nzauwazasz, ze nadepnales na cialo 50 letniego mezczyzny w stanie rozkladu. Kucasz,\nby mu sie przyjrzec, lecz nie zauwazasz nic nadzwyczajnego. Podnosisz glowe i w oddali\nzauwazasz ledwo swiecacy ekran. Gdy sie do niego zblizasz, odczytujesz, ze jest to\nczytnik linii papilarnych, do ktorego przykladasz dlon - niestety na ekranie pojawia\nsie komunikat \"brak dostepu\". Stojac wciaz w tym samym miejscu i powstrzymujac odruchy\nwymiotne, wytezasz wzrok i w przeciwleglym rogu dostrzegasz kontury narzedzi stolarskich:\npile mechaniczna, siekiere, papier scierny i roznego rodzaju pilniki."
  putStrLn output
  game gameState

lookAround5 :: GameState -> IO() --TODO not modular not implemented
lookAround5 gameState = do
  let output = "Pusty opis"
  putStrLn output
  game gameState

addElementToEq :: [String] -> String -> [String]
addElementToEq inventoryState item = do
  if item == "worek" then do inventoryState ++ ["czerwona-przekladnia", "niebieska-przekladnia", "zielona-przekladnia"]
    else do inventoryState ++ [item]

addElementToRoom :: [String] -> String -> [String]
addElementToRoom roomState item = do
  roomState ++ [item]

pickUp :: GameState -> String -> IO()
pickUp gameState item = do
  let roomId = first gameState
      inventoryState = second gameState
      roomsState = third gameState
      counters = fourth gameState
      currentRoomState = roomsState !! roomId

  if isOnList item currentRoomState then do --check if on the list
    if item == "cialo" || item == "cialo-bez-reki" then do
      putStrLn "Nie mozna podniesc ciala - jest za ciezkie."
      game gameState
      else do
        let newCurrentRoomState = removeFromList item currentRoomState --remove item currentRoom
            newRoomsState = replaceNth roomId newCurrentRoomState roomsState --update roomsState
            newInventoryState = addElementToEq inventoryState item
            newGameState = (roomId, newInventoryState, newRoomsState, counters) --update gameState
            output = "Podnosisz " ++ item
        putStrLn output
        game newGameState
    else do
      let output = item ++ " nie ma w pokoju"
      putStrLn output
      game gameState

readNote :: GameState -> String -> IO()
readNote gameState item = do
  let roomId = first gameState
      inventoryState = second gameState
  if isOnList item inventoryState then do
    if item == "list" then do
      putStrLn "Witaj podrozniku!\nJesli czytasz ten list prawdopodobnie jestes w takiej samej sytuacji jak ja teraz. Notatka jest zdradliwa, aby otworzyc drzwi musisz podac prawidlowy kod, jednak notatka nie precyzuje dokladnie jaki jest kod. Niestety po 3 probie wpisanie kodu mechanizm zablokowal sie, a na wyswietlaczu pojawil sie licznik. Po kilku dniach zorientowalem sie, ze licznik pokazuje liczbe dni do jakiegos wydarzenia. (Byc moze do oblokowania mechanizmu) Zostaly mi jeszcze 83 dni. Jesli to czytasz chcialbym Cie poinformowac, ze poczatkowe liczby pierwsze, ktore wyznaczylem do obliczenia kodu to 11, 13 i 29. Niestety zadna z nich nie jest prawidlowa.\nSprawdz inne liczby. Moze Ci sie uda.\nPowodzenia!"
      game gameState
    else if item == "notatka" then do
      putStrLn "Wez liczbe pierwsza pomiedzy 10 a 30, podnies ja do kwadratu, a nastepnie pomnoz przez liczbe pelnych tygodni w kazdym roku."
      game gameState
    else if roomId == 2 && item == "papier" then do
      putStrLn "Tam, gdzie pod szafirowym niebem\nrubinowe pola kwiatow kwitna\nlaka sie mieni, jakby szmaragd wielki,\nkuszac strudzonych, by przycuplneli"
      game gameState
    else do
      let output = "Nie mozesz przeczytac " ++ item
      putStrLn output
      game gameState
  else do
    let output = "Nie posiadasz " ++ item
    putStrLn output
    game gameState
    
incorrectUsage :: GameState -> String -> String -> IO()
incorrectUsage gameState item roomObject = do
  let output = "Nie mozna uzyc " ++ item ++ " z obiektem " ++ roomObject
  putStrLn output
  game gameState

use0 :: GameState -> String -> String -> IO()
use0 gameState item roomObject = do
  let inventoryState = second gameState
      roomsState = third gameState
      counters = fourth gameState
  if item == "klucz" && roomObject == "drzwi" then do
    let newInventoryState = removeFromList item inventoryState
        newGameState = (1, newInventoryState, roomsState, counters)
    lookAround newGameState
    else incorrectUsage gameState item roomObject

use3 :: GameState -> String -> String -> IO()
use3 gameState item roomObject = do
  let inventoryState = second gameState
      roomsState = third gameState
      counters = fourth gameState
  if item == "wytrych" && roomObject == "drzwi" then do
    let newInventoryState = removeFromList item inventoryState
        newGameState = (4, newInventoryState, roomsState, counters)
    lookAround newGameState
  else incorrectUsage gameState item roomObject

use4 :: GameState -> String -> String -> IO()
use4 gameState item roomObject = do
  let inventoryState = second gameState
      roomsState = third gameState
      counters = fourth gameState
      currentRoomState = roomsState !! 4
  if item == "pila" || item == "siekiera" && roomObject == "cialo" then do
    let newCurrentRoomState = removeFromList roomObject currentRoomState --remove roomObject currentRoom
        newCurrentRoomState2 = addElementToRoom newCurrentRoomState "cialo-bez-reki"
        newCurrentRoomState3 = addElementToRoom newCurrentRoomState2 "reka"
        newRoomsState = replaceNth 4 newCurrentRoomState3 roomsState --update roomsState
        newGameState = (4, inventoryState, newRoomsState, counters)
    putStrLn "Odciales reke od ciala"
    game newGameState
  else if item == "reka" && roomObject == "czytnik" then do
    let newInventoryState = removeFromList item inventoryState
        newGameState = (5, newInventoryState, roomsState, counters)
    lookAround newGameState
  else do
    incorrectUsage gameState item roomObject

use :: GameState -> String -> String -> IO()
use gameState item roomObject = do
  let roomId = first gameState
      inventoryState = second gameState
  if isOnList item inventoryState then do
    case roomId of
      0 -> use0 gameState item roomObject
      3 -> use3 gameState item roomObject
      4 -> use4 gameState item roomObject
      _ -> incorrectUsage gameState item roomObject
  else do
    let output = "Nie posiadasz przedmiotu " ++ item
    putStrLn output
    game gameState

craft :: GameState -> String -> String -> IO()
craft gameState item1 item2 = do
  let roomId = first gameState
      inventoryState = second gameState
      roomsState = third gameState
      counters = fourth gameState
  if isOnList item1 inventoryState && isOnList item2 inventoryState then do
    if (item1 == "drut" && item2 == "blaszka") || (item1 == "blaszka" && item2 == "drut") then do
      let newInventoryState = removeFromList item1 inventoryState
          newInventoryState2 = removeFromList item2 newInventoryState
          newInventoryState3 = newInventoryState2 ++ ["wytrych"]
          newGameState = (roomId, newInventoryState3, roomsState, counters)
      putStrLn "Zrobiles wytrych"
      game newGameState
--    else if roomId == 1 && item == "xd" && roomObject == "xd2" then do
--      game gameState
    else do
      let output = "Nie mozna polaczyc " ++ item1 ++ " z " ++ item2
      putStrLn output
      game gameState
  else do
    let output = "Nie posiadasz " ++ item1 ++ " lub " ++ item2
    putStrLn output
    game gameState

showEq :: GameState -> IO()
showEq gameState = do
  let inventoryState = second gameState
  print inventoryState
  game gameState

enterCode :: GameState -> String -> IO()
enterCode gameState code = do
  let roomId = first gameState
      inventoryState = second gameState
      roomsState = third gameState
      counters = fourth gameState
      triesLeftCode = counters !! roomId
  if code == "27508" then do
    putStrLn "bzzz: PRAWIDLOWY KOD" 
    let newGameState = (roomId + 1, inventoryState, roomsState, counters)
    lookAround newGameState
  else if triesLeftCode == 0 then do
    putStrLn "bzzz: ZLY KOD"
    putStrLn "52"
    putStrLn "Koniec gry: Umarles z glodu!"
  else do
    let newCounters = incrementCounter (-1) roomId counters
        newGameState = (roomId, inventoryState, roomsState, newCounters) --update gameState
    putStrLn "bzzz: ZLY KOD"
    game newGameState
    
incrementCounter :: Int -> Int -> [Int] -> [Int]
incrementCounter value roomId counters = do
  let counterValue = counters !! roomId --TODO check if exists
      newValue = counterValue + value
  replaceNth roomId newValue counters --update counters

help :: GameState -> IO()
help gameState = do
  putStrLn "=============="
  putStrLn "Dostepne komendy:"
  putStrLn "- rozejrzyj sie\n- podnies <przedmiot>\n- przeczytaj <przedmiot>\n- uzyj\n- polacz <przedmiot 1> <przedmiot 2>\n- przegladaj ekwipunek\n- wpisz kod <kod>\n- pomocy\n- koniec"
  putStrLn "=============="
  game gameState

wrongCommand :: GameState -> IO()
wrongCommand gameState = do
  putStrLn "Bledna komenda!"
  help gameState

command :: [String] -> GameState -> IO ()
command line gameState = do
  if length line > 3 then wrongCommand gameState
  else do
    let cmd = head line
    case cmd of
      "rozejrzyj" -> if length line == 1 || (length line == 2 && (line !! 1) == "sie") then lookAround gameState else wrongCommand gameState
      "podnies" -> if length line == 2 then pickUp gameState (line !! 1) else wrongCommand gameState
      "przeczytaj" -> if length line == 2 then readNote gameState (line !! 1) else wrongCommand gameState
      "uzyj" -> use gameState (line !! 1) (line !! 2)
      "polacz" -> if length line == 3 then craft gameState (line !! 1) (line !! 2) else wrongCommand gameState
      "przegladaj" -> if length line == 1 || (length line == 2 && (line !! 1) == "ekwipunek") then showEq gameState else wrongCommand gameState
      "wpisz" -> if length line == 3 && (line !! 1) == "kod" then enterCode gameState (line !! 2) else wrongCommand gameState
      "pomocy" -> help gameState
      "koniec" -> exitSuccess
      _ -> do wrongCommand gameState

gameOver :: GameState -> Bool
gameOver gameState = first gameState == 5

game :: GameState -> IO()
game gameState = do
    if gameOver gameState then putStrLn "Koniec gry!"
    else do
      line <- getInputLine "Co robisz?";
      let tokenizedLine = tokenize line
      command tokenizedLine gameState

main :: IO ()
main = lookAround initialGameState