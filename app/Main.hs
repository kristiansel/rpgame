module Main where

import System.Environment
import System.IO
import Character
import GameState

main :: IO ()
main = do
  putStrLn "Welcome to the world of RPGame!"
  args <- getArgs
  character <- parseArgs args
  
  let init_game_state = initGameState character
  endState <- evolveGameState init_game_state

  return ()

parseArgs :: [String] -> IO Character
parseArgs [filename] = tryParseChrFile filename
parseArgs (filename:xs) = do
  putStrLn "more than one argument given, ignoring rest"
  tryParseChrFile filename
parseArgs [] = createCharacter
