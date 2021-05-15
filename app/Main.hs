module Main where

import System.IO (hFlush, stdout)
import System.Exit
import Prelude

type GameState = (Int, [String], [[String]], [Int]) -- (nr pokoju, inventory, [p1, p2, p3...], counters)
type WorldDescription = [String]

initialGameState :: GameState
initialGameState = (3, [], [["klucz"], ["notatka", "list"], ["papier"], ["drut", "blaszka"], ["pila", "siekiera", "papier", "cialo"]], [0, 2]) --TODO change inital state to 0

worldDescription :: WorldDescription
worldDescription = ["Ocknales sie. Lezysz na podlodze w dziwnym pomieszczeniu. Pierwszy raz je widzisz. Wstajesz i przecierasz oczy. To nie jest sen. Na scianie przed Toba widnieje namazany czerwona substacja napis: 'Nie ma ratunku!'. W pokoju znajduje sie jeszce stolik oraz wielkie czerwone drzwi. Podchodzisz... Na stole lezy klucz.",
  "Przechodzisz do drugiego pokoju. Na podlodze lezy szkielet. Chyba jest to szkielet Twojego poprzednika, ktoremu nie udalo sie uciec. Szkielet trzyma w rece jakies zawiniatko - chyba jest to jakis list. Dodatkowo widzisz jeszcze stolik, na ktorym lezy notatka. Kolejne drzwi sa zamkniete jednak zamiast tradycyjnego klucza potrzebujesz wpisac kod.",
  "Wchodzisz do kolejnego pokoju. Po Twojej lewej stronie stoi regal pelen ksiazek, zas z prawej widzisz trzy kolorowe dzwignie - niebieska, zielona i czerwona. Pod regalem znajduje sie sterta brudnych ubran, a posrod nich mozna rowniez zobaczyc dlugopis bez skuwki, zgnieciony papier, paczke zapalek i kilka drobnych monet.",
  "W nastepnym pomieszczeniu nic nie widac. Wszedzie unosi sie dym. Jednak nie masz wyjscia, wchodzisz do pomieszczenia zaslaniajac usta i nos rekami. Drzwi zatrzaskuja sie za Toba. Po omacku badasz pomieszczenie. Na podlodze lezy kawalek drutu, blaszka, butelka z woda i recznik. Moczysz recznik woda i tworzysz z niego cos w rodzaju maski. Teraz mozesz ekplorowac dalej. Znajdujesz drzwi. Nie ma zamka, zamiast niego natrafiasz na stara klodke.",
  "Po odblokowaniu drzwi pedzisz dalej co sil w nogach, by tylko nabrac do pluc swiezego powietrza. Niestety, gdy opuszczasz kleby dymu, do Twoich nozdrzy dociera okropny smrod. Dookola panuje mrok. Nie jestes w stanie wytrzymac odoru i zwracasz swoje sniadanie wprost przed siebie. Po chwili decydujesz sie przejsc po omacku dalej, ale Twoja noga trafia na cos miekkiego. Gdy twoj wzrok przyzwyczaja sie do ciemnosci, zauwazasz, ze nadepnales na cialo 50 letniego mezczyzny w stanie rozkladu. Kucasz, by mu sie przyjrzec, lecz nie zauwazasz nic nadzwyczajnego. Podnosisz glowe i w oddali zauwazasz ledwo swiecacy ekran. Gdy sie do niego zblizasz, odczytujesz, ze jest to czytnik linii papilarnych, do ktorego przykladasz dlon - niestety na ekranie pojawia sie komunikat \"brak dostepu\". Stojac wciaz w tym samym miejscu i powstrzymujac odruchy wymiotne, wytezasz wzrok i w przeciwleglym rogu dostrzegasz kontury narzedzi stolarskich - pile mechaniczna, siekiere, papier scierny i roznego rodzaju pilniki."]

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
        output = "Podnosisz " ++ item
    putStrLn output
    game newGameState
  else do
    let output = item ++ " nie ma w pokoju"
    putStrLn output
    game gameState

readNote :: GameState -> String -> IO()
readNote gameState item = do
  let roomId = first(gameState)
  let inventoryState = second(gameState)
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
    else if roomId == 3 && item == "wytrych" && roomObject == "drzwi" then do
      let newInventoryState = removeFromList item inventoryState
          newGameState = (roomId + 1, newInventoryState, roomsState, counters)
      lookAround newGameState
      game gameState
    else do
      let output = "Nie mozna uzyc " ++ item ++ " z obiektem " ++ roomObject
      putStrLn output
      game gameState
  else do
    let output = "Nie posiadasz przedmiotu " ++ item
    putStrLn output
    game gameState

craft :: GameState -> String -> String -> IO()
craft gameState item1 item2 = do
  let roomId = first(gameState)
      inventoryState = second(gameState)
      roomsState = third(gameState)
      counters = fourth(gameState)
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
      "rozejrzyj" -> if (length line) == 1 || ((length line) == 2 && (line !! 1) == "sie") then lookAround gameState
                      else wrongCommand gameState
      "podnies" -> if (length line) == 2 then pickUp gameState (line !! 1) else wrongCommand gameState
      "przeczytaj" -> if (length line) == 2 then readNote gameState (line !! 1) else wrongCommand gameState
      "uzyj" -> use gameState (line !! 1) (line !! 2)
      "polacz" -> if (length line) == 3 then craft gameState (line !! 1) (line !! 2) else wrongCommand gameState
      "przegladaj" -> if (length line) == 1 then showEq gameState else wrongCommand gameState
      "wpisz" -> if (length line) == 3 && (line !! 1) == "kod" then enterCode gameState (line !! 2)
                 else wrongCommand gameState
      "pomocy" -> help gameState
      "koniec" -> exitSuccess
      _ -> do wrongCommand gameState

gameOver :: GameState -> Bool
gameOver gameState = first(gameState) == 5

game :: GameState -> IO()
game gameState = do
    if gameOver gameState then putStrLn "Koniec gry!"
    else do
      line <- getInputLine "Co robisz?";
      let tokenizedLine = tokenize line
      command tokenizedLine gameState

main :: IO ()
main = lookAround initialGameState