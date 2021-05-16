module Main where
  
import System.IO (hFlush, stdout)
import System.Exit
import Prelude

{-# LANGUAGE FlexibleInstances #-}

type GameState = (Int, [String], [[String]], [Int], [Int]) -- (nr pokoju, inventory, [p1, p2, p3...], counters, sequence)

initialGameState :: GameState

initialGameState = (0, [], [["klucz"], ["notatka", "list"], ["papier"], ["drut", "blaszka"], ["piła", "siekiera", "papier-ścierny", "ciało"], ["xd"]], [0, 2, 0, 0, 0, 0], [0, 0, 0]) --TODO change inital state to 0

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
    _ -> do putStrLn "Pokój nie istnieje"

lookAround0 :: GameState -> IO()
lookAround0 gameState = do
  let roomId = first gameState
      roomState = third gameState !! roomId
      outputPart1 = "Ocknąłeś się. Leżysz na podłodze w dziwnym pomieszczeniu. Pierwszy raz je widzisz.\nWstajesz i przecierasz oczy. To nie jest sen. Na ścianie przed Tobą widnieje\nnamazany czerwoną substacją napis: \"Nie ma ratunku!\". W pokoju znajduje się jeszcze\nstolik oraz wielkie czerwone drzwi."
      outputPart2 = " Podchodzisz bliżej... Na stole leży klucz."
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
      outputPart1 = "Przechodzisz do drugiego pokoju, a za sobą słyszysz dźwięk zamykających się drzwi.\nNa podłodze leży szkielet. Prawdopodobnie jest to truchło Twojego poprzednika,\nktóremu nie udało się uciec."
      outputPart2 = " Kościana ręka trzyma jakieś zawiniątko - wygląda to\nna jakiś list."
      outputPart3 = " Dodatkowo widzisz jeszcze stolik, na którym leży notatka."
      outputPart4 = "\nKolejne drzwi są zamknięte, jednak zamiast tradycyjnego klucza potrzebujesz wpisać kod."
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
  let output = "Wchodzisz do kolejnego pokoju - droga powrotu jest zablokowana. Po Twojej lewej\nstronie stoi regał pełen książek, zaś z prawej widzisz zestaw trzech kolorowych\ndźwigni - niebieską, zieloną i czerwoną. Pod regałem znajduje się sterta\nbrudnych ubrań, a pośród nich można również zobaczyć długopis bez skuwki,\nzgnieciony papier, paczkę zapałek i kilka drobnych monet."
  putStrLn output
  game gameState

lookAround3 :: GameState -> IO() --TODO not modular not implemented
lookAround3 gameState = do
  let output = "W następnym pomieszczeniu nic nie widać. Wszędzie unosi sie dym. Jednak, po zatrzaśnięciu\npoprzednich drzwi, nie masz wyjścia, wchodzisz do pomieszczenia zasłaniając usta i nos\nrękami. Drzwi zatrzaskują się za Tobą. Po omacku badasz pomieszczenie. Na podłodze leży\nkawałek drutu, blaszka, butelka z wodą i ręcznik. Moczysz ręcznik wodą i tworzysz z niego\ncoś w rodzaju maski. Teraz możesz eksplorować dalej. Znajdujesz drzwi. Nie ma zamka,\nzamiast niego natrafiasz na starą kłódkę."
  putStrLn output
  game gameState

lookAround4 :: GameState -> IO() --TODO not modular not implemented
lookAround4 gameState = do
  let output = "Po odblokowaniu drzwi pędzisz do kolejnego pomieszczenia. Ponownie, tak jak\ni wcześniej, drzwi za Tobą zatrzaskują się. W następnym pokoju na przeciwległej\nścianie widzisz drzwi oraz świecący ekran. Gdy się do niego zbliżasz, odczytujesz,\nże jest to czytnik linii papilarnych, do którego przykładasz dłoń - niestety,\nna ekranie pojawia się komunikat \"brak dostępu\". Rozglądasz się uważniej\npo pomieszczeniu i widzisz mnóstwo elementów wskazujących na to, iż był to jakiś\nwarsztat. Na stole znajdują się narzędzia stolarskie: piła mechaniczna, siekiera,\npapier ścierny i różnego rodzaju pilniki. Na ścianach znajdują się obrazy pewnego\nmężczyzny, a w kącie wiele ukończonych i nieukończonych wynalazków. Przyglądasz\nim się uważnie i dostrzegasz fascynację wynalazcy anatomią. Być może eksperymentował\nnad stworzeniem nowej protezy lub egzoszkieletu. Po dłuższej chwili obserwujesz\nw drugim rogu pokoju krzesło, na którym znajduje się coś na kształt człowieka,\nto chyba kolejny eksperyment! Przyglądasz się mu dokładniej i widzisz bardzo\nprecyzyjnie wykonaną twarz i dłonie, które wyglądają jak u żywego człowieka."
  putStrLn output
  game gameState

lookAround5 :: GameState -> IO() --TODO not modular not implemented
lookAround5 gameState = do
  let output = "Pusty opis"
  putStrLn output
  game gameState

addElementToEq :: [String] -> String -> [String]
addElementToEq inventoryState item = do
  inventoryState ++ [item]

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
    if item == "ciało" || item == "ciało-bez-reki" then do
      putStrLn "Nie można podnieść ciała - jest za ciężkie."
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
      putStrLn "Witaj podróżniku!\nJeśli czytasz ten list, prawdopodobnie jesteś w takiej samej sytuacji jak ja teraz."
      putStrLn "Notatka jest zdradliwa, aby otworzyć drzwi, musisz podać prawidłowy kod, jednak notatka nie precyzuje"
      putStrLn "dokładnie jaki jest kod. Niestety, po trzeciej próbie wpisania kodu, mechanizm zablokował się, a na"
      putStrLn "wyświetlaczu pojawił się licznik. Po kilku dniach zorientowałem się, że licznik pokazuje liczbę dni"
      putStrLn "do jakiegoś wydarzenia (być może do oblokowania mechanizmu). Zostały mi jeszcze 83 dni. Jeśli to czytasz,"
      putStrLn "chciałbym Cię poinformować, że początkowe liczby pierwsze, które wyznaczyłem do obliczenia kodu to: 11, 13"
      putStrLn "i 29. Niestety, żadna z nich nie jest prawidłowa. Sprawdź inne liczby. Może Ci się uda.\nPowodzenia!"
      game gameState
    else if item == "notatka" then do
      putStrLn "Weź liczbę pierwszą pomiędzy 10 a 30, podnieś ją do kwadratu, a następnie pomnóż przez liczbę pełnych"
      putStrLn "tygodni w każdym roku."
      game gameState
    else if roomId == 2 && item == "papier" then do
      putStrLn "Tam, gdzie pod szafirowym niebem\nrubinowe pola kwiatów kwitną\nłąka sie mieni, jakby szmaragd wielki,"
      putStrLn "kusząc strudzonych, by przycupnęli"
      game gameState
    else do
      let output = "Nie możesz przeczytać " ++ item
      putStrLn output
      game gameState
  else do
    let output = "Nie posiadasz " ++ item
    putStrLn output
    game gameState
    
incorrectUsage :: GameState -> String -> String -> IO()
incorrectUsage gameState item roomObject = do
  let output = "Nie można użyć " ++ item ++ " z obiektem " ++ roomObject
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
  if item == "piła" || item == "siekiera" && roomObject == "ciało" then do
    let newCurrentRoomState = removeFromList roomObject currentRoomState --remove roomObject currentRoom
        newCurrentRoomState2 = addElementToRoom newCurrentRoomState "ciało-bez-ręki"
        newCurrentRoomState3 = addElementToRoom newCurrentRoomState2 "ręka"
        newRoomsState = replaceNth 4 newCurrentRoomState3 roomsState --update roomsState
        newGameState = (4, inventoryState, newRoomsState, counters, fifth gameState)
    putStrLn "Odciąłeś rękę od ciała"
    game newGameState
  else if item == "ręka" && roomObject == "czytnik" then do
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
        lookAround (first gameState + 1, second gameState, third gameState, fourth gameState, fifth gameState)
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
      putStrLn "Zrobiłes wytrych"
      game newGameState
--    else if roomId == 1 && item == "xd" && roomObject == "xd2" then do
--      game gameState
    else do
      let output = "Nie mozna połączyć " ++ item1 ++ " z " ++ item2
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
    putStrLn "bzzz: PRAWIDŁOWY KOD"
    let newGameState = (roomId + 1, inventoryState, roomsState, counters, fifth gameState)
    lookAround newGameState
  else if triesLeftCode == 0 then do
    putStrLn "bzzz: ZŁY KOD"
    putStrLn "52"
    putStrLn "Koniec gry: Umarłeś z głodu!"
  else do
    let newCounters = incrementCounter (-1) roomId counters
        newGameState = (roomId, inventoryState, roomsState, newCounters, fifth gameState) --update gameState
    putStrLn "bzzz: ZŁY KOD"
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
  putStrLn "- rozejrzyj się\n- podnieś <przedmiot>\n- przeczytaj <przedmiot>\n- użyj\n- połącz <przedmiot 1> <przedmiot 2>"
  putStrLn "- przeglądaj ekwipunek\n- wpisz kod <kod>\n- pomocy\n- koniec"
  putStrLn "=============="
  game gameState

wrongCommand :: GameState -> IO()
wrongCommand gameState = do
  putStrLn "Błędna komenda!"
  help gameState

command :: [String] -> GameState -> IO ()
command line gameState = do
  if length line > 3 then wrongCommand gameState
  else do
    let cmd = head line
    case cmd of
      "rozglądam" -> if length line == 1 || (length line == 2 && (line !! 1) == "się") then lookAround gameState else wrongCommand gameState
      "podnoszę" -> if length line == 2 then pickUp gameState (line !! 1) else wrongCommand gameState
      "czytam" -> if length line == 2 then readNote gameState (line !! 1) else wrongCommand gameState
      "używam" -> if length line == 2 then use gameState (line !! 1) else if length line == 3 then use2Items gameState (line !! 1) (line !! 2) else wrongCommand gameState
      "łączę" -> if length line == 3 then craft gameState (line !! 1) (line !! 2) else wrongCommand gameState
      "przeglądam" -> if length line == 1 || (length line == 2 && (line !! 1) == "ekwipunek") then showEq gameState else wrongCommand gameState
      "wpisuję" -> if length line == 3 && (line !! 1) == "kod" then enterCode gameState (line !! 2) else wrongCommand gameState
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