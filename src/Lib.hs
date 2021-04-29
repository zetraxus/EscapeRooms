module Lib
    ( gameStart,
      getCommand
    ) where

import Data.Text as T
import System.IO

getCommand :: String -> IO String
getCommand prompt = do
    putStrLn prompt
    hFlush stdout
    getLine

abracadabra :: String -> String
abracadabra hehehehe = do
  l <- getLine
  putStrLn l
  4
--  command2 <- T.unpack command
--  command3 <- T.splitOn " " command

gameStart :: IO ()
gameStart = do
  putStrLn "TODO poczatek historii"
  command <- getCommand "co robisz?"
  show "temp"
  4
--  trolo <- abracadabra

--  option <- getLine
--  temp <- words "co robisz?"
--  show temp
--  show temp

--"rozejrzyj sie" -- + 0 arg
--"podnies" -- + 1 arg
--"przeczytaj" -- + 1 arg
--"uzyj" -- + 2 arg
--"ekwipunek" -- + 0 arg
--"wpisz kod" -- + 1 arg
--
--prefix sentence 6:
--  slowo1 -> fun1()
--  slowo2 -> fun2()
--prefix sentence 7:
--  slowoo1 -> fun3()
--  slowoo2 ->
--
--przeczytaj arg =
--dfpsjdslfsd
--fdslfjsdl

