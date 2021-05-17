module Utils
    (removeFromList, replaceNthElement, isOnList, getInputLine) where

import System.IO (hFlush, stdout)

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