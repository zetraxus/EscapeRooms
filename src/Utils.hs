module Utils
    (removeFromList, replaceNthElement, isOnList, getInputLine) where

import System.IO (hFlush, stdout)

--Funkcja usuwająca element z listy.
--Parametry wejściowe: element do usunięcia, lista elementów.
--Parametry wyjściowe: nowa lista.
removeFromList :: Eq a => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList x (y:ys) | x == y = ys
                        | otherwise = y : removeFromList x ys

--Funkcja pozwalająca na zamianę elementu na liście.
--Parametry wejściowe: indeks elementu do zamiany, nowy element, lista elementów.
--Parametry wyjściowe: nowa lista.
replaceNthElement :: Int -> a -> [a] -> [a]
replaceNthElement _ _ [] = []
replaceNthElement n newVal (x:xs) | n == 0 = newVal:xs
                                  | otherwise = x:replaceNthElement (n-1) newVal xs

--Funkcja pozwalająca na sprawdzenie czy element jest na liście.
--Parametry wejściowe: element do sprawdzenia, lista elementów.
--Parametry wyjściowe: wartość logiczna opisująca czy dany element jest na liście.
isOnList :: Eq a => a -> [a] -> Bool
isOnList _ [] = False 
isOnList x (y:ys) | x == y = True
                  | otherwise = isOnList x ys

--Funkcja pobierająca ze standardowego wejścia input gracza.
--Parametry wejściowe: prompt.
--Parametry wyjściowe: input gracza.
getInputLine :: String -> IO String
getInputLine prompt = do
    putStrLn prompt
    hFlush stdout
    getLine