module Main where

import Lib
import System.IO (hFlush, stdout)
import Data.Text as T
--
getCommand :: String -> IO String
getCommand prompt = do
    putStrLn prompt
    hFlush stdout
    getLine

checkCommand :: String -> Int
checkCommand command = case T.splitOn (T.pack " ") (T.pack command) !! 0 of
      T.pack "nic" -> 0
      T.pack "cos" -> 1
    

--head :: [a] -> a
--head (x:_) = x

main = do {
--    str <- getLine;
--    putStrLn str;
    command <- getCommand "Co robisz?";
    checkCommand command;
--    T.splitOn (T.pack " ") (T.pack command) !! 0;
    --    T.unpack $ T.splitOn (T.pack " ") (T.pack command);
    putStrLn command;
}