module Main where

import Lib
import System.IO (hFlush, stdout)
import Data.Text as T
import Data.IORef

type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do
   r <- newIORef 0
   return (\i -> do modifyIORef r (+i)
                    readIORef r)

getCommand :: String -> IO String
getCommand prompt = do
    putStrLn prompt
    hFlush stdout
    getLine

lookAround :: IO()
lookAround = putStrLn "lookAround"

pickUp :: IO()
pickUp = putStrLn "pickUp"

readNote :: IO()
readNote = putStrLn "readNote"

use :: IO()
use = putStrLn "use"

showEq :: IO()
showEq = putStrLn "showEq"

enterCode :: IO()
enterCode = putStrLn "enterCode"

main = do {
    room_id <- makeCounter;
--    a <- room_id 3;
--    print [a];
    command <- getCommand "Co robisz?";
    if command == "rozgladam sie"
      then lookAround
      else if command == "podnosze"
        then pickUp
        else if command == "czytam"
          then readNote
          else if command == "uzywam"
            then use
            else if command == "przegladam eq"
              then showEq
              else if command == "wpisuje kod"
                then enterCode
                else if command == "ratunku!"
                  then putStrLn "Co robisz? \n rozgladam sie \n podnosze \n czytam \n uzywam \n przegladam eq \n wpisuje kod"
                  else putStrLn "Błędna komenda";
--    print $ T.splitOn (T.pack " ") (T.pack command) !! 0;
--    checkCommand $ T.splitOn (T.pack " ") (T.pack command) !! 0;
--    T.splitOn (T.pack " ") (T.pack command) !! 0;
--    T.unpack $ T.splitOn (T.pack " ") (T.pack command);
}
