module GameState ( GameState,
                   initGameState,
                   evolveGameState ) where

import Prompt
import Character
import System.IO

type GameState = (Character, Int)

data Command = Continue | Quit

initGameState :: Character -> IO GameState
initGameState character = return (character, 0)

tryParseCommand :: String -> Maybe Command
tryParseCommand "c" = Just Continue
tryParseCommand "q" = Just Quit
tryParseCommand _  = Nothing

promptCommand :: IO Command
promptCommand = promptData "Command:" tryParseCommand "unrecognized command, c = continue, q = quit"

evolveGameState :: IO GameState -> IO GameState
evolveGameState io_gamestate = do
  (character, num) <- io_gamestate
  putStrLn ("Game iteration: " ++ show num)
  command <- promptCommand
  case command of
    Quit -> return (character, num)
    Continue -> evolveGameState $ return (character, num + 1)
