module Main where
  
import System.IO (hFlush, stdout)
import System.Exit
import Prelude

{-# LANGUAGE FlexibleInstances #-}

-- (room id, inventory, [room_1 items, room_2 items, ...], counters, sequence)
type GameState = (Int, [String], [[String]], [Int], [Int])

initialGameState :: GameState
initialGameState = (0, [], [["klucz"], ["notatka", "list"], ["papier"], ["drut", "blaszka"], ["piła", "siekiera", "papier-ścierny", "ciało"]], [0, 2, 0, 0, 0], [0, 0, 0])

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

removeFromList :: Eq a => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList x (y:ys) | x == y = ys
                        | otherwise = y : removeFromList x ys

replaceNthElement :: Int -> a -> [a] -> [a]
replaceNthElement _ _ [] = []
replaceNthElement n newVal (x:xs) | n == 0 = newVal:xs
                                  | otherwise = x:replaceNthElement (n-1) newVal xs
                           
isOnList :: Eq a => a -> [a] -> Bool
isOnList _ [] = False 
isOnList x (y:ys) | x == y = True
                  | otherwise = isOnList x ys

getInputLine :: String -> IO String
getInputLine prompt = do
    putStrLn prompt
    hFlush stdout
    getLine

printGameState :: GameState -> IO()
printGameState gameState = do
  print $ getRoomId gameState
  print $ getInventory gameState
  print $ getItemsInCurrentRoom gameState
  print $ getCounters gameState
  print $ getSequence gameState

lookAround :: GameState -> IO()
lookAround gameState = do
  case getRoomId gameState of
    0 -> lookAround0 gameState
    1 -> lookAround1 gameState
    2 -> lookAround2 gameState
    3 -> lookAround3 gameState
    4 -> lookAround4 gameState
    _ -> do putStrLn "Pokój nie istnieje"

lookAround0 :: GameState -> IO()
lookAround0 gameState = do
  let msg1 = "Ocknąłeś się. Leżysz na podłodze w dziwnym pomieszczeniu, które widzisz pierwszy raz w życiu.\nWstajesz \
              \i przecierasz oczy. To nie jest sen. Na ścianie przed Tobą widnieje\nnamazany czerwoną substacją napis:\
              \\"Nie ma ratunku!\". W pokoju znajduje się jeszcze\ndrewniany stolik oraz wielkie czerwone drzwi."
      msg2 = " Podchodzisz bliżej do stołu... Leży na nim niewielki klucz."
  if isOnList "klucz" (getItemsInCurrentRoom gameState) then
    putStrLn $ msg1 ++ msg2
  else
    putStrLn msg1
  game gameState

lookAround1 :: GameState -> IO()
lookAround1 gameState = do
  let roomItems = getItemsInCurrentRoom gameState
      msg1 = "Przechodzisz do drugiego pokoju, a za sobą słyszysz dźwięk zamykających się drzwi.\nNa podłodze leży \
              \szkielet. Być może był to Twój poprzednik,\nktóremu nie udało się uciec. Zaczynasz rozumieć, że znajdujesz\
              \się w jakimś więzieniu, z którego należy jak najszybciej uciec."
      msg2 = " Kościana ręka trzyma jakieś zawiniątko - wygląda to\nna jakiś list."
      msg3 = " Dodatkowo widzisz jeszcze stolik, na którym leży notatka."
      msg4 = "\nKolejne drzwi są zamknięte, jednak zamiast standardowej kłódki potrzebujesz wpisać kod."
  if isOnList "list" roomItems && isOnList "notatka" roomItems then do
    putStrLn $ msg1 ++ msg2 ++ msg3 ++ msg4
  else if isOnList "notatka" roomItems then do
    putStrLn $ msg1 ++ msg3 ++ msg4
  else if isOnList "list" roomItems then do
    putStrLn $ msg1 ++ msg2 ++ msg4
  else do
    putStrLn $ msg1 ++ msg4
  game gameState

lookAround2 :: GameState -> IO() --TODO not modular not implemented
lookAround2 gameState = do
  let output = "Wchodzisz do kolejnego pokoju - droga powrotu jest zablokowana. Po Twojej lewej\nstronie stoi regał\
                \pełen książek, zaś z prawej widzisz zestaw trzech kolorowych\ndźwigni - niebieską, zieloną i czerwoną.\
                \Pod regałem znajduje się sterta\nbrudnych ubrań, a pośród nich można również zobaczyć długopis bez \
                \skuwki,\nzgnieciony papier, paczkę zapałek i kilka drobnych monet."
  putStrLn output
  game gameState

lookAround3 :: GameState -> IO() --TODO not modular not implemented
lookAround3 gameState = do
  let output = "W następnym pomieszczeniu nic nie widać. Wszędzie unosi sie dym. Jednak, po zatrzaśnięciu\npoprzednich\
                \drzwi, nie masz wyjścia, wchodzisz do pomieszczenia zasłaniając usta i nos\nrękami. Drzwi zatrzaskują\
                \się za Tobą. Po omacku badasz pomieszczenie. Na podłodze leży\nkawałek drutu, blaszka, butelka z wodą\
                \i ręcznik. Moczysz ręcznik wodą i tworzysz z niego\ncoś w rodzaju maski. Teraz możesz eksplorować dalej.\
                \Znajdujesz drzwi. Nie ma zamka,\nzamiast niego natrafiasz na starą kłódkę."
  putStrLn output
  game gameState

lookAround4 :: GameState -> IO() --TODO not modular not implemented
lookAround4 gameState = do
  let output = "Po odblokowaniu drzwi pędzisz do kolejnego pomieszczenia. Ponownie, tak jak\ni wcześniej, drzwi za Tobą\
                \zatrzaskują się. W następnym pokoju na przeciwległej\nścianie widzisz drzwi oraz świecący ekran. Gdy\
                \się do niego zbliżasz, odczytujesz,\nże jest to czytnik linii papilarnych, do którego przykładasz dłoń\
                \- niestety,\nna ekranie pojawia się komunikat \"brak dostępu\". Rozglądasz się uważniej\npo \
                \pomieszczeniu i widzisz mnóstwo elementów wskazujących na to, iż był to jakiś\nwarsztat. Na stole\
                \znajdują się narzędzia stolarskie: piła mechaniczna, siekiera,\npapier ścierny i różnego rodzaju\
                \pilniki. Na ścianach znajdują się obrazy pewnego\nmężczyzny, a w kącie wiele ukończonych i\
                \nieukończonych wynalazków. Przyglądasz\nim się uważnie i dostrzegasz fascynację wynalazcy anatomią.\
                \Być może eksperymentował\nnad stworzeniem nowej protezy lub egzoszkieletu. Po dłuższej chwili\
                \obserwujesz\nw drugim rogu pokoju krzesło, na którym znajduje się coś na kształt człowieka,\nto chyba\
                \kolejny eksperyment! Przyglądasz się mu dokładniej i widzisz bardzo\nprecyzyjnie wykonaną twarz i\
                \dłonie, które wyglądają jak u żywego człowieka."
  putStrLn output
  game gameState

pickUp :: GameState -> String -> IO()
pickUp gameState item = do
  if isOnList item (getItemsInCurrentRoom gameState) then do
    if getRoomId gameState == 4 && (item == "ciało" || item == "ciało-bez-reki") then do
      putStrLn "Nie można podnieść manekina - jest za ciężki."
      game gameState
    else do
      let newCurrentRoomState = removeFromList item (getItemsInCurrentRoom gameState)
          newRoomsState = replaceNthElement (getRoomId gameState) newCurrentRoomState (getRoomsStates gameState)
          newInventoryState = item : getInventory gameState
          newGameState = (getRoomId gameState, newInventoryState, newRoomsState, getCounters gameState, getSequence gameState)
      putStrLn $ "Podnosisz " ++ item ++ " i umieszczasz go w swoim ekwipunku."
      game newGameState
    else do
      putStrLn $ "Nie możesz podnieść przedmiotu " ++ item
      game gameState

readNote :: GameState -> String -> IO()
readNote gameState item = do
  if isOnList item (getInventory gameState) then do
    if item == "list" then do
      putStrLn "Witaj podróżniku!\nJeśli czytasz ten list, prawdopodobnie jesteś w takiej samej sytuacji jak ja teraz.\
              \\nNotatka jest zdradliwa, aby otworzyć drzwi, musisz podać prawidłowy kod, jednak notatka nie precyzuje\
              \\ndokładnie jaki jest kod. Niestety, po trzeciej próbie wpisania kodu, mechanizm zablokował się, a na\n\
              \wyświetlaczu pojawił się licznik. Po kilku dniach zorientowałem się, że licznik pokazuje liczbę dni\n\
              \do jakiegoś wydarzenia (być może do oblokowania mechanizmu). Zostały mi jeszcze 83 dni. Jeśli to czytasz\
              \,\nchciałbym Cię poinformować, że początkowe liczby pierwsze, które wyznaczyłem do obliczenia kodu to:\
              \11, 13\ni 29. Niestety, żadna z nich nie jest prawidłowa. Sprawdź inne liczby. Mam nadzieję że chociaż\
              \Tobie się to uda.\nPowodzenia!"
      game gameState
    else if item == "notatka" || item == "notatke" || item == "notatkę" then do
      putStrLn "Weź liczbę pierwszą pomiędzy 10 a 30, podnieś ją do kwadratu, a następnie pomnóż przez liczbę pełnych\n\
                \tygodni w każdym roku."
      game gameState
    else if item == "papier" then do
      putStrLn "Tam, gdzie pod szafirowym niebem\nrubinowe pola kwiatów kwitną\nłąka sie mieni, jakby szmaragd wielki,\
                \\nkusząc strudzonych, by przycupnęli."
      game gameState
    else do
      putStrLn $ "Nie możesz przeczytać przedmiotu " ++ item
      game gameState
  else do
    putStrLn $ "Nie posiadasz w swoim ekwipunku przedmiotu " ++ item
    game gameState
    
incorrectUsage :: GameState -> String -> String -> IO()
incorrectUsage gameState item roomObject = do
  putStrLn ("Nie można użyć " ++ item ++ " z obiektem " ++ roomObject)
  game gameState

use0 :: GameState -> String -> String -> IO()
use0 gameState item roomObject = do
  if item == "klucz" && roomObject == "drzwi" then do
    let newInventoryState = removeFromList item (getInventory gameState)
        newGameState = (getRoomId gameState + 1, newInventoryState, getRoomsStates gameState, getCounters gameState, getSequence gameState)
    lookAround newGameState
    else incorrectUsage gameState item roomObject

use3 :: GameState -> String -> String -> IO()
use3 gameState item roomObject = do
  if item == "wytrych" && roomObject == "kłódka" then do
    putStrLn "Po kilku sekundach udało Ci się otworzyć wytrychem kłódkę."
    let newInventoryState = removeFromList item (getInventory gameState)
        newGameState = (getRoomId gameState + 1, newInventoryState, getRoomsStates gameState, getCounters gameState, getSequence gameState)
    lookAround newGameState
  else incorrectUsage gameState item roomObject
-- TODO use4
use4 :: GameState -> String -> String -> IO()
use4 gameState item roomObject = do
  let roomId = getRoomId gameState
      inventoryState = getInventory gameState
      roomsState = getRoomsStates gameState
  if item == "piła" || item == "siekiera" && roomObject == "ciało" then do
    let newCurrentRoomState = removeFromList roomObject (getItemsInCurrentRoom gameState)
        newCurrentRoomState2 = "ciało-bez-ręki" : "ręka" : newCurrentRoomState
        newRoomsState = replaceNthElement roomId newCurrentRoomState2 roomsState
        newGameState = (getRoomId gameState, inventoryState, newRoomsState, getCounters gameState, getSequence gameState)
    putStrLn "Odciąłeś rękę od ciała"
    game newGameState
  else if item == "ręka" && roomObject == "czytnik" then do
    let newInventoryState = removeFromList item inventoryState
        newGameState = (roomId + 1, newInventoryState, roomsState, getCounters gameState, getSequence gameState)
    lookAround newGameState
  else do
    incorrectUsage gameState item roomObject

use2Items :: GameState -> String -> String -> IO()
use2Items gameState item roomObject = do
  if isOnList item (getInventory gameState) then do
    case getRoomId gameState of
      0 -> use0 gameState item roomObject
      3 -> use3 gameState item roomObject
      4 -> use4 gameState item roomObject
      _ -> incorrectUsage gameState item roomObject
  else do
    putStrLn $ "Nie posiadasz w swoim ekwipunku przedmiotu " ++ item
    game gameState

useLever :: GameState -> [Int] -> Int -> [Int] -> IO()
useLever gameState sequence leverIndex correctSequence = do
  if sequence !! leverIndex > 0 then do
   putStrLn "Przeciągnąłeś już wcześniej tę dźwignię. Nic nowego tym razem się nie wydarzyło."
   game gameState
  else do
    putStrLn "Przeciągnąłeś wybraną dźwignię."
    if sequence == correctSequence then do
      let newSequence = replaceNthElement leverIndex 1 sequence
      if newSequence == [1, 1, 1] then do
        putStrLn "Była to poprawna sekwencja. Powoli otwierają się drzwi do kolejnego pomieszczenia!"
        lookAround (getRoomId gameState + 1, getInventory gameState, getRoomsStates gameState, getCounters gameState, getSequence gameState)
      else do
        game (getRoomId gameState, getInventory gameState, getRoomsStates gameState, getCounters gameState, newSequence)
    else do
      let newSequence = replaceNthElement leverIndex 2 sequence
      if isOnList 0 newSequence then
        game (getRoomId gameState, getInventory gameState, getRoomsStates gameState, getCounters gameState, newSequence)
      else do
        putStrLn "Pociągnąłeś ostatnią dźwignię. Po chwili widzisz jak każda z nich wraca na początkową pozycję.\n\
                  \Możesz spróbować ponownie."
        game (getRoomId gameState, getInventory gameState, getRoomsStates gameState, getCounters gameState, [0, 0, 0])

use :: GameState -> String -> IO()
use gameState item = do
  if getRoomId gameState == 2 then do
    case item of
      "czerwona-dźwignia" -> useLever gameState (getSequence gameState) 0 [0, 1, 0]
      "niebieska-dźwignia" -> useLever gameState (getSequence gameState) 1 [0, 0, 0]
      "zielona-dźwignia" -> useLever gameState (getSequence gameState) 2 [1, 1, 0]
      _ -> incorrectUsage gameState item ""
  else incorrectUsage gameState item ""

craft :: GameState -> String -> String -> IO()
craft gameState item1 item2 = do
  let inventoryState = getInventory gameState
  if isOnList item1 inventoryState && isOnList item2 inventoryState then do
    if (item1 == "drut" && item2 == "blaszka") || (item1 == "blaszka" && item2 == "drut") then do
      let newInventoryState = removeFromList item1 inventoryState
          newInventoryState2 = removeFromList item2 newInventoryState
          newInventoryState3 = "wytrych" : newInventoryState2
          newGameState = (getRoomId gameState, newInventoryState3, getRoomsStates gameState, getCounters gameState, getSequence gameState)
      putStrLn "Zrobiłes wytrych"
      game newGameState
    else do
      putStrLn $ "Nie mozna połączyć przedmiotu " ++ item1 ++ " z przedmiotem " ++ item2
      game gameState
  else do
    putStrLn $ "Nie posiadasz w swoim ekwipunku przedmiotu " ++ item1 ++ " lub " ++ item2
    game gameState

showInventory :: GameState -> IO()
showInventory gameState = do
  print $ getInventory gameState
  game gameState

enterCode :: GameState -> String -> IO()
enterCode gameState code = do
  let roomId = getRoomId gameState
      counters = getCounters gameState
      triesLeftCode = counters !! roomId
  if code == "27508" then do
    putStrLn "bzzz: PRAWIDŁOWY KOD"
    lookAround (getRoomId gameState + 1, getInventory gameState, getRoomsStates gameState, counters, getSequence gameState)
  else if triesLeftCode == 0 then do
    putStrLn "bzzz: ZŁY KOD\n52\nKoniec gry: Umarłeś z głodu!"
  else do
    let newCounters = incrementCounter (-1) roomId counters
    putStrLn "bzzz: ZŁY KOD"
    game (getRoomId gameState, getInventory gameState, getRoomsStates gameState, newCounters, getSequence gameState)
    
incrementCounter :: Int -> Int -> [Int] -> [Int]
incrementCounter value roomId counters = do
  let counterValue = counters !! roomId --TODO check if exists
      newValue = counterValue + value
  replaceNthElement roomId newValue counters --update counters

help :: GameState -> IO()
help gameState = do
  putStrLn "============== \n\
            \Dostepne komendy: \n\
            \- rozglądam się \n\
            \- podnoszę <przedmiot> \n\
            \- upuszczam <przedmiot> \n\
            \- czytam <przedmiot> \n\
            \- używam <przedmiot> \n\
            \- używam <przedmiot> <przedmiot> \n\
            \- łączę <przedmiot 1> <przedmiot 2> \n\
            \- przeglądam ekwipunek \n\
            \- wpisuję kod <kod> \n\
            \- pomocy \n\
            \- koniec \n\
            \=============="
  game gameState

wrongCommand :: GameState -> IO()
wrongCommand gameState = do
  putStrLn "Błędna komenda! Spróbuj jeszcze raz."
  help gameState

dropItem :: GameState -> String -> IO()
dropItem gameState item = do
  if isOnList item (getInventory gameState) then do
    let newInventory = removeFromList item (getInventory gameState)
        newRoomState = item : getItemsInCurrentRoom gameState
        newRoomsState = replaceNthElement (getRoomId gameState) newRoomState (getRoomsStates gameState)
    putStrLn $ "Upuszczasz " ++ item
    game (getRoomId gameState, newInventory, newRoomsState, getCounters gameState, getSequence gameState)
   else do
    putStrLn "Nie masz przedmiotu o takiej nazwie w swoim ekwipunku"
    game gameState

command :: [String] -> GameState -> IO ()
command line gameState = do
  if length line > 3 then wrongCommand gameState
  else do
    case head line of
      "upuszczam" -> if length line == 2 
        then dropItem gameState (line !! 1) 
        else wrongCommand gameState
      "rozglądam" -> if length line == 1 || (length line == 2 && (line !! 1) == "się")
        then lookAround gameState
        else wrongCommand gameState
      "podnoszę" -> if length line == 2
        then pickUp gameState (line !! 1)
        else wrongCommand gameState
      "czytam" -> if length line == 2
        then readNote gameState (line !! 1)
        else wrongCommand gameState
      "używam" -> if length line == 2
        then use gameState (line !! 1)
        else if length line == 3 
          then use2Items gameState (line !! 1) (line !! 2)
          else wrongCommand gameState
      "łączę" -> if length line == 3 
        then craft gameState (line !! 1) (line !! 2)
        else wrongCommand gameState
      "przeglądam" -> if length line == 1 || (length line == 2 && (line !! 1) == "ekwipunek") 
        then showInventory gameState
        else wrongCommand gameState
      "wpisuję" -> if length line == 3 && (line !! 1) == "kod"
        then enterCode gameState (line !! 2) 
        else wrongCommand gameState
      "pomocy" -> help gameState
      "koniec" -> exitSuccess
      _ -> do wrongCommand gameState

gameOver :: GameState -> Bool
gameOver gameState = getRoomId gameState == length (getRoomsStates gameState)

game :: GameState -> IO()
game gameState = do
    if gameOver gameState then putStrLn "Koniec gry!"
    else do
      line <- getInputLine "Co teraz robisz?";
      command (words line) gameState

main :: IO ()
main = lookAround initialGameState