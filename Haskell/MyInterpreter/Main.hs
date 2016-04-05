module Main where

import Interpreter
import Control.Monad (when)
import System.IO (stdin,hGetLine,hFlush,Handle,stdout)
import Data.Char (isSpace)

main :: IO ()
main = putStrLn "welcome!" >> process stdin

process :: Handle -> IO ()
process h
 = do
   putStr "? "
   hFlush stdout
   xs <- hGetLine h
   if null xs then putStrLn helpStr >> process h
      else if head xs == ':'
         then case take 2 xs of
               ":?" -> putStrLn helpStr >> process h
               ":q" -> putStrLn "bye!"
               ":r" -> case dropWhile isSpace (dropWhile (not.isSpace) xs) of
                       "" -> putStrLn "ERROR - No name specified"
                       str ->
                           do
                           exp <- catch (readFile str) (\e -> return "")
                           if not (null exp)
                              then do putStrLn $ show $ eval exp
                                      process h
                              else do putStrLn ("ERROR - Unable to open file \"" ++ str ++ "\"")
                                      process h
               _    -> putStrLn "no such directive" >> process h
      else do putStrLn $ show $ eval xs
              process h

helpStr
 = "Commands:\n" ++
   "   :q                  Quit.\n" ++
   "   :?                  Print this message.\n" ++
   "   expression          Evaluate this expression.\n" ++
   "   :c                  Compile this expression.\n" ++
   "   :r filename         Read commands from file.\n"
