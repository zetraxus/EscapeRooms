module Main where
  
import System.IO (hFlush, stdout)
import System.Exit
import Prelude

{-# LANGUAGE FlexibleInstances #-}

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
    _ -> lookAroundFinal gameState 

lookAround0 :: GameState -> IO()
lookAround0 gameState = do
  let msg1 = "Ocknąłeś się. Leżysz na podłodze w dziwnym pomieszczeniu, które widzisz pierwszy raz w życiu.\nWstajesz \
              \i przecierasz oczy. To nie jest sen. Na ścianie przed Tobą widnieje namazany czerwoną\nsubstacją napis: \
              \\"Nie ma ratunku!\". W pokoju znajduje się jeszcze drewniany stolik oraz wielkie \nczerwone drzwi."
      msg2 = " Podchodzisz bliżej do stołu... Leży na nim niewielki klucz."
  if isOnList "klucz" (getItemsInCurrentRoom gameState) then
    putStrLn $ msg1 ++ msg2
  else
    putStrLn msg1
  game gameState

lookAround1 :: GameState -> IO()
lookAround1 gameState = do
  let roomItems = getItemsInCurrentRoom gameState
      msg1 = "Przechodzisz do drugiego pokoju, a za sobą słyszysz dźwięk zamykających się drzwi. Na podłodze \nleży \
              \szkielet. Być może był to Twój poprzednik, któremu nie udało się uciec. \nZaczynasz rozumieć, \
              \że znajdujesz się w jakimś więzieniu, z którego należy jak najszybciej uciec.\n"
      msg2 = "Kościana ręka trzyma jakieś zawiniątko - wygląda to na jakiś list."
      msg3 = "\nOprócz tego widzisz jeszcze stolik, na którym leży notatka."
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

lookAround2 :: GameState -> IO()
lookAround2 gameState = do
  let roomItems = getItemsInCurrentRoom gameState
      msg1 = "Wchodzisz do kolejnego pokoju - droga powrotu jest zablokowana. Po Twojej lewej stronie stoi regał\
              \\npełen książek, zaś z prawej widzisz zestaw trzech kolorowych dźwigni - niebieską, zieloną i czerwoną.\
              \\nPod regałem znajduje się sterta brudnych ubrań, a pośród nich można również zobaczyć długopis bez\
              \\nskuwki, "
      msg2 = "zgniecioną kartkę, "
      msg3 = "paczkę zapałek i kilka drobnych monet."
  if isOnList "kartka" roomItems then do
    putStrLn $ msg1 ++ msg2 ++ msg3
  else do
    putStrLn $ msg1 ++ msg3
  game gameState

lookAround3 :: GameState -> IO()
lookAround3 gameState = do
  let roomItems = getItemsInCurrentRoom gameState
      msg1 = "W następnym pomieszczeniu nic nie widzisz. Wszędzie unosi sie dym. Po zatrzaśnięciu poprzednich drzwi\
              \\nwiesz, że nie masz powrotu, wchodzisz zasłaniając usta i nos rękami. Po omacku badasz pomieszczenie.\
              \\nNa podłodze leży "
      msg2 = "kawałek drutu, "
      msg3 = "blaszka, "
      msg4 = "butelka z wodą i ręcznik. Moczysz ręcznik wodą i tworzysz z niego coś w rodzaju maski.\
              \\nTeraz możesz eksplorować dalej. Znajdujesz drzwi. Nie posiadają\
              \\none zamka, zamiast tego zauważasz starą kłódkę."
  if isOnList "drut" roomItems && isOnList "blaszka" roomItems then do
    putStrLn $ msg1 ++ msg2 ++ msg3 ++ msg4
  else if isOnList "blaszka" roomItems then do
    putStrLn $ msg1 ++ msg3 ++ msg4
  else if isOnList "drut" roomItems then do
    putStrLn $ msg1 ++ msg2 ++ msg4
  else do
    putStrLn $ msg1 ++ msg4
  game gameState

lookAround4 :: GameState -> IO()
lookAround4 gameState = do
  let roomItems = getItemsInCurrentRoom gameState
  let msg1 = "Po odblokowaniu drzwi pędzisz do kolejnego pomieszczenia. Ponownie, tak jak i wcześniej, \ndrzwi za Tobą\
              \zatrzaskują się. W następnym pokoju na przeciwległej ścianie widzisz drzwi \noraz świecący ekran. Gdy\
              \się do niego zbliżasz, zauważasz, że jest to czytnik linii papilarnych, \ndo którego natychmiast \
              \przykładasz dłoń - niestety, na ekranie pojawia się komunikat \n\"brak dostępu\". Rozglądasz się \
              \uważniej po pomieszczeniu i widzisz mnóstwo elementów \nwskazujących na to, iż był to jakiś warsztat. \
              \Na stole znajdują się narzędzia stolarskie: \n"
      msg2 = "piła, "
      msg3 = "siekiera, "
      msg4 = "młotek i różnego rodzaju pilniki. Na \
              \ścianach znajdują się obrazy pewnego \nmężczyzny, a w kącie wiele ukończonych i nieukończonych \
              \wynalazków. Przyglądasz się im \nuważnie i dostrzegasz fascynację wynalazcy anatomią. Być może \
              \eksperymentował nad stworzeniem \nnowej protezy lub egzoszkieletu. Po dłuższej chwili obserwujesz w \
              \drugim rogu pokoju krzesło, \nna którym znajduje się coś na kształt człowieka, to chyba jego kolejny \
              \eksperyment. \nPrzyglądasz się dokładniej manekinowi i widzisz bardzo precyzyjnie wykonaną twarz, \
              \wyglądającą \njak osoba z otaczających Cię obrazów"
      msg5 = ", i dłonie, które wyglądają jak u żywego człowieka"
      msg6 = "."
  if isOnList "piła" roomItems && isOnList "siekiera" roomItems && isOnList "manekin" roomItems then do
    putStrLn $ msg1 ++ msg2 ++ msg3 ++ msg4 ++ msg5 ++ msg6
  else if isOnList "piła" roomItems && isOnList "siekiera" roomItems then do
    putStrLn $ msg1 ++ msg2 ++ msg3 ++ msg4 ++ msg6
  else if isOnList "piła" roomItems && isOnList "manekin" roomItems then do
    putStrLn $ msg1 ++ msg2 ++ msg4 ++ msg5 ++ msg6
  else if isOnList "siekiera" roomItems && isOnList "manekin" roomItems then do
    putStrLn $ msg1 ++ msg3 ++ msg4 ++ msg5 ++ msg6
  else if isOnList "piła" roomItems then do
    putStrLn $ msg1 ++ msg2 ++ msg4 ++ msg6
  else if isOnList "siekiera" roomItems then do
    putStrLn $ msg1 ++ msg3 ++ msg4 ++ msg6
  else if isOnList "manekin" roomItems then do
    putStrLn $ msg1 ++ msg4 ++ msg5 ++ msg6
  else do
    putStrLn $ msg1 ++ msg4 ++ msg6
  game gameState

lookAroundFinal :: GameState -> IO()
lookAroundFinal gameState = do
  let finalMsg = "Udało Ci się! Wreszcie wyszedłeś na wolność. "
  if isOnList "sztabka" (getInventory gameState) then do
    putStrLn $ finalMsg ++ "Udało Ci się zachować swoją sztabkę złota. Jesteś bogaty, gratulacje!"
  else
    putStrLn finalMsg
    
pickUp :: GameState -> String -> IO()
pickUp gameState item = do
  if isOnList item (getInventory gameState) then do
    putStrLn "Już posiadasz ten przedmiot w ekwipunku."
    game gameState
  else if isOnList item (getItemsInCurrentRoom gameState) then do
    if getRoomId gameState == 4 && (item == "manekin" || item == "manekin-bez-ręki") then do
      putStrLn "Nie można podnieść manekina - jest zbyt ciężki."
      game gameState
    else do
      if length (getInventory gameState) == 3 then do
        putStrLn "Osiągnąłeś limit pojemności plecaka. Nie możesz podnieść kolejnego przedmiotu."
        game gameState
      else do
        let newCurrentRoomState = removeFromList item (getItemsInCurrentRoom gameState)
            newRoomsState = replaceNthElement (getRoomId gameState) newCurrentRoomState (getRoomsStates gameState)
            newInventoryState = item : getInventory gameState
            newGameState = (getRoomId gameState, newInventoryState, newRoomsState, getCounters gameState, getSequence gameState)
        putStrLn $ "Podnosisz " ++ item ++ " i umieszczasz go w swoim ekwipunku."
        game newGameState
  else do
    putStrLn $ "Nie możesz podnieść przedmiotu " ++ item ++ "."
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
              \11, 13\ni 29. Niestety, żadna z nich nie jest prawidłowa. Sprawdź inne liczby. Mam nadzieję że chociaż \
              \Tobie się to uda.\nPowodzenia!"
      game gameState
    else if item == "notatka" then do
      putStrLn "Weź liczbę pierwszą pomiędzy 10 a 30, podnieś ją do kwadratu, a następnie pomnóż przez liczbę pełnych\n\
                \tygodni w każdym roku."
      game gameState
    else if item == "kartka" then do
      putStrLn "Tam, gdzie pod szafirowym niebem\nrubinowe pola kwiatów kwitną\nłąka sie mieni, jakby szmaragd wielki,\
                \\nkusząc strudzonych, by przycupnęli."
      game gameState
    else do
      putStrLn $ "Nie możesz przeczytać przedmiotu " ++ item ++ "."
      game gameState
  else do
    putStrLn $ "Nie posiadasz w swoim ekwipunku przedmiotu " ++ item ++ "."
    game gameState
    
incorrectUsage :: GameState -> String -> String -> IO()
incorrectUsage gameState item roomObject = do
  if roomObject /= "" then
    putStrLn $ "Nie można użyć przedmiotu " ++ item ++ " z obiektem " ++ roomObject ++ "."
  else
    putStrLn $ "Nie można użyć przedmiotu " ++ item ++ "."
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

use4 :: GameState -> String -> String -> IO()
use4 gameState item roomObject = do
  let roomId = getRoomId gameState
      inventoryState = getInventory gameState
      roomsState = getRoomsStates gameState
  if (item == "piła" || item == "siekiera") && roomObject == "manekin" then do
    let newCurrentRoomState = removeFromList roomObject (getItemsInCurrentRoom gameState)
        newCurrentRoomState2 = "manekin-bez-ręki" : "dłoń" : newCurrentRoomState
        newRoomsState = replaceNthElement roomId newCurrentRoomState2 roomsState
        newGameState = (getRoomId gameState, inventoryState, newRoomsState, getCounters gameState, getSequence gameState)
    putStrLn "Odciąłeś dłoń manekina."
    game newGameState
  else if item == "dłoń" && roomObject == "czytnik" then do
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
    putStrLn $ "Nie posiadasz w swoim ekwipunku przedmiotu " ++ item ++ "."
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
        let roomId = getRoomId gameState
            newCounters = incrementCounter (-1) roomId (getCounters gameState)
            triesLeftCode = newCounters !! roomId
        if triesLeftCode == 0 then do
            putStrLn "Próbując pociągnąć ostatnią dźwignię doszło do jej złamania. Nigdy już stąd nie wyjdziesz. Koniec gry."
        else do
          putStrLn "Pociągnąłeś ostatnią dźwignię. Po chwili widzisz jak każda z nich wraca na początkową pozycję.\n\
                  \Możesz spróbować ponownie."
          game (getRoomId gameState, getInventory gameState, getRoomsStates gameState, newCounters, [0, 0, 0])

useLever :: GameState -> String -> IO()
useLever gameState item = do
  if getRoomId gameState == 2 then do
    case item of
      "czerwona" -> useLever gameState (getSequence gameState) 0 [0, 1, 0]
      "czerwoną" -> useLever gameState (getSequence gameState) 0 [0, 1, 0]
      "niebieska" -> useLever gameState (getSequence gameState) 1 [0, 0, 0]
      "niebieską" -> useLever gameState (getSequence gameState) 1 [0, 0, 0]
      "zielona" -> useLever gameState (getSequence gameState) 2 [1, 1, 0]
      "zieloną" -> useLever gameState (getSequence gameState) 2 [1, 1, 0]
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
      putStrLn "Zrobiłes wytrych."
      game newGameState
    else do
      putStrLn $ "Nie mozna połączyć przedmiotu " ++ item1 ++ " z przedmiotem " ++ item2 ++ "."
      game gameState
  else do
    putStrLn $ "Nie posiadasz w swoim ekwipunku przedmiotu " ++ item1 ++ " lub " ++ item2 ++ "."
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
  let counterValue = counters !! roomId
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
            \- łączę <przedmiot> <przedmiot> \n\
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
    putStrLn $ "Upuszczasz " ++ item ++ "."
    game (getRoomId gameState, newInventory, newRoomsState, getCounters gameState, getSequence gameState)
   else do
    putStrLn "Nie masz przedmiotu o takiej nazwie w swoim ekwipunku."
    game gameState

command :: [String] -> GameState -> IO ()
command line gameState = do
  let dropItemNames = ["upuszczam", "upuść", "upusc"]
  let lookAroundNames = ["rozglądam", "rozgladam", "rozejrzyj"]
  let pickUpNames = ["podnoszę", "podnosze", "podnieś", "podnies"]
  let readNoteNames = ["czytam", "przeczytaj", "czytaj"]
  let useNames = ["używam", "uzywam", "użyj", "uzyj"]
  let leverNames = ["dźwignię", "dźwignie", "dzwignię", "dzwignie", "dźwignia", "dzwignia", "dźwigni", "dzwigni"]
  let craftNames = ["łączę", "łącze", "łaczę", "łacze", "lączę", "lącze", "laczę", "lacze", "połącz", "połacz", "polącz", "polacz"]
  let showInventoryNames = ["przeglądam", "przegladam", "przejrzyj", "obejrzyj", "zobacz"]
  let enterCodeNames = ["wpisuję", "wpisuje", "wpisz"]
  let helpNames = ["pomocy", "pomoc", "help"]
  let exitNames = ["koniec", "exit", "wyjście", "wyjscie", "wyjdź"]

  if length line > 3 then do wrongCommand gameState
  else if isOnList (head line) dropItemNames then do
     if length line == 2 then dropItem gameState (line !! 1)
     else wrongCommand gameState
  else if isOnList (head line) lookAroundNames then do
    if length line == 1 || (length line == 2 && ((line !! 1) == "się" || (line !! 1) == "sie")) then lookAround gameState
    else wrongCommand gameState
  else if isOnList (head line) pickUpNames then do
    if length line == 2 then pickUp gameState (line !! 1)
    else wrongCommand gameState
  else if isOnList (head line) readNoteNames then do
    if length line == 2 then readNote gameState (line !! 1)
    else wrongCommand gameState
  else if isOnList (head line) useNames then do
    if isOnList (line !! 1) leverNames
    then use gameState (line !! 2)
    else if length line == 3
         then use2Items gameState (line !! 1) (line !! 2)
         else wrongCommand gameState
  else if isOnList (head line) craftNames then do
    if length line == 3 then craft gameState (line !! 1) (line !! 2)
    else wrongCommand gameState
  else if isOnList (head line) showInventoryNames then do
    if length line == 1 || (length line == 2 && (line !! 1) == "ekwipunek") then showInventory gameState
    else wrongCommand gameState
  else if isOnList (head line) enterCodeNames then do
    if length line == 3 && (line !! 1) == "kod" then enterCode gameState (line !! 2)
    else wrongCommand gameState
  else if isOnList (head line) helpNames then do help gameState
  else if isOnList (head line) exitNames then do exitSuccess
  else do wrongCommand gameState

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