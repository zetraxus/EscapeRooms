module Main where
  
import System.IO (hFlush, stdout)
import System.Exit
import Prelude

{-# LANGUAGE FlexibleInstances #-}

type GameState = (Int, [String], [[String]], [Int], [Int]) -- (nr pokoju, inventory, [p1, p2, p3...], counters, sequence)

initialGameState :: GameState

initialGameState = (0, [], [["klucz"], ["notatka", "list"], ["papier", "worek"], ["drut", "blaszka"], ["pila", "siekiera", "papier-scierny", "cialo"], ["xd"]], [0, 2, 0, 0, 0, 0], [0, 0, 0])

first :: (a, b, c, d, e) -> a
first (a, _, _, _, _) = a

second :: (a, b, c, d, e) -> b
second (_, b, _, _, _) = b

third :: (a, b, c, d, e) -> c
third (_, _, c, _, _) = c

fourth :: (a, b, c, d, e) -> d
fourth (_, _, _, d, _) = d
  
fifth :: (a, b, c, d, e) -> e
fifth (_, _, _, _, e) = e

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
  let output = "Po odblokowaniu drzwi pedzisz do kolejnego pomieszczenia. Ponownie tak jak i wcześniej, drzwi za Tobą zatrzaskują się. W następnym pokoju na przeciwległej ścianie drzwi oraz świecący ekran. Gdy sie do niego zblizasz, odczytujesz, ze jest to czytnik linii papilarnych, do którego przykładasz dłoń - niestety na ekranie pojawia się komunikat \"brak dostepu\". Rozglądasz się uważniej po pokoju i widzisz mnóstwo elementów wskazujących na to, iż był to jakiś warsztat. Na stole znajdują się narzedza stolarskie:\npila mechaniczna, siekiera, papier scierny i roznego rodzaju pilniki. Na ścianach znajduje się wiele obrazów pewnego mężczyzny, a w kącie wiele ukończonych i nieukończonych wynalazków. Przyglądasz im się uważnie i dostrzegasz fascynację wynalazcy anatomią. Być może eksperymentował nad stworzeniem nowej protezy lub egzoszkieletu. Po dłuższej chwili obserwujesz w drugim rogu pokoju krzesło, na którym znajduje się coś na kształt człowieka, to chyba kolejny eksperyment! Przyglądasz się mu dokładniej i widzisz bardzo precyzyjnie wykonaną twarz i dłonie, które wyglądają jak u żywego człowieka."
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
            newGameState = (roomId, newInventoryState, newRoomsState, counters, fifth gameState) --update gameState
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
        newGameState = (1, newInventoryState, roomsState, counters, fifth gameState)
    lookAround newGameState
    else incorrectUsage gameState item roomObject

use3 :: GameState -> String -> String -> IO()
use3 gameState item roomObject = do
  let inventoryState = second gameState
      roomsState = third gameState
      counters = fourth gameState
  if item == "wytrych" && roomObject == "drzwi" then do
    let newInventoryState = removeFromList item inventoryState
        newGameState = (4, newInventoryState, roomsState, counters, fifth gameState)
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
        newGameState = (4, inventoryState, newRoomsState, counters, fifth gameState)
    putStrLn "Odciales reke od ciala"
    game newGameState
  else if item == "reka" && roomObject == "czytnik" then do
    let newInventoryState = removeFromList item inventoryState
        newGameState = (5, newInventoryState, roomsState, counters, fifth gameState)
    lookAround newGameState
  else do
    incorrectUsage gameState item roomObject

use2Items :: GameState -> String -> String -> IO()
use2Items gameState item roomObject = do
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

compareSequences :: GameState -> [Int] -> Int -> [Int] -> IO()
compareSequences gameState sequence leverIndex correctSequence = do
  if sequence !! leverIndex > 0 then do
   putStrLn "Przeciągnąłeś już wcześniej tę dźwignię. Nic nowego tym razem się nie wydarzyło."
   game gameState
  else do
    putStrLn "Przeciągnąłeś wybraną dźwignię."
    if sequence == correctSequence then do
      let newSequence = replaceNth leverIndex 1 sequence
      if newSequence == [1, 1, 1] then do
        putStrLn "Była to poprawna sekwencja. Powoli otwierają się drzwi do kolejnego pomieszczenia!"
        game (first gameState + 1, second gameState, third gameState, fourth gameState, fifth gameState)
      else do
        game (first gameState, second gameState, third gameState, fourth gameState, newSequence)
    else do
      let newSequence = replaceNth leverIndex 2 sequence
      if isOnList 0 newSequence then
        game (first gameState, second gameState, third gameState, fourth gameState, newSequence)
      else do
        putStrLn "Pociągnąłeś ostatnią dźwignię. Po chwili widzisz jak każda z nich wraca na początkową pozycję."
        putStrLn "Możesz spróbować ponownie."
        game (first gameState, second gameState, third gameState, fourth gameState, [0, 0, 0])

-- niebieski czerwony zielony
use :: GameState -> String -> IO()
use gameState item = do
  let roomId = first gameState
      sequence = fifth gameState
  if roomId == 2 then do
    case item of
      "cz" -> compareSequences gameState sequence 0 [0, 1, 0]
      "ni" -> compareSequences gameState sequence 1 [0, 0, 0]
      "zi" -> compareSequences gameState sequence 2 [1, 1, 0]
      _ -> incorrectUsage gameState item ""
  else incorrectUsage gameState item ""

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
          newGameState = (roomId, newInventoryState3, roomsState, counters, fifth gameState)
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
    let newGameState = (roomId + 1, inventoryState, roomsState, counters, fifth gameState)
    lookAround newGameState
  else if triesLeftCode == 0 then do
    putStrLn "bzzz: ZLY KOD"
    putStrLn "52"
    putStrLn "Koniec gry: Umarles z glodu!"
  else do
    let newCounters = incrementCounter (-1) roomId counters
        newGameState = (roomId, inventoryState, roomsState, newCounters, fifth gameState) --update gameState
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

dropItem :: GameState -> String -> IO()
dropItem gameState item = do
  let roomId = first gameState
      inventoryState = second gameState
      roomsState = third gameState
      currentRoomState = roomsState !! roomId

  if isOnList item inventoryState then do --check if on the list
    let newInventory = removeFromList item inventoryState --remove item currentRoom
        newRoomState = currentRoomState ++ [item]
        newRoomsState = replaceNth roomId newRoomState roomsState
        newGameState = (roomId, newInventory, newRoomsState, fourth gameState, fifth gameState) --update gameState
        output = "Upuszczasz " ++ item
    putStrLn output
    game newGameState
   else do
    putStrLn "nie masz przedmiotu o takiej nazwie w ekwipunku"
    game gameState

command :: [String] -> GameState -> IO ()
command line gameState = do
  if length line > 3 then wrongCommand gameState
  else do
    let cmd = head line
    case cmd of
      "rozgladam" -> if length line == 1 || (length line == 2 && (line !! 1) == "sie") then lookAround gameState else wrongCommand gameState
      "podnosze" -> if length line == 2 then pickUp gameState (line !! 1) else wrongCommand gameState
      "upuszczam" -> if length line == 2 then dropItem gameState (line !! 1) else wrongCommand gameState -- TODO add to help
      "czytam" -> if length line == 2 then readNote gameState (line !! 1) else wrongCommand gameState
      "uzywam" -> if length line == 2 then use gameState (line !! 1) else if length line == 3 then use2Items gameState (line !! 1) (line !! 2) else wrongCommand gameState
      "lacze" -> if length line == 3 then craft gameState (line !! 1) (line !! 2) else wrongCommand gameState
      "przegladam" -> if length line == 1 || (length line == 2 && (line !! 1) == "ekwipunek") then showEq gameState else wrongCommand gameState
      "wpisuje" -> if length line == 3 && (line !! 1) == "kod" then enterCode gameState (line !! 2) else wrongCommand gameState
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