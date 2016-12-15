module Prompt ( promptData ) where

import System.IO

type ParseFuncType a = (String -> Maybe a)

promptData :: String -> ParseFuncType a -> String -> IO a
promptData sprompt parseFunc failstr = do
  putStr sprompt
  hFlush stdout
  input <- getLine
  let maybea = parseFunc input
  case maybea of
    Just something -> return something
    Nothing -> do
      putStrLn failstr
      promptData sprompt parseFunc failstr
