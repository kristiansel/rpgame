module Character
  (
    Character,
    createCharacter,
    tryParseChrFile
  ) where

import Prompt
import System.IO

data Gender = Male | Female deriving(Show)
data Character = Character String Gender

-- charName :: Character -> String
-- charName (Character name _) = name

tryParseGender :: String -> Maybe Gender
tryParseGender gender_str = case gender_str of
  "m" -> Just Male
  "f" -> Just Female
  _   -> Nothing

tryParseName :: String -> Maybe String
tryParseName "" = Nothing
tryParseName s = Just s

promptDataGender :: IO Gender
promptDataGender = promptData "Gender (m/f):" tryParseGender "Gender must be either 'm' or 'f'"

promptDataName :: IO String
promptDataName = promptData "Name:" tryParseName "Name must contain at least one charcter"

createCharacter :: IO Character
createCharacter = do
  putStrLn "Create your character."
  name <- promptDataName
  gender <- promptDataGender

  let gender_str = show gender
  putStrLn ("Character: " ++ name ++ ['(', head gender_str, ')'])
  let filename = name ++ ".chr"
  writeFile filename ("N:" ++ name ++ ", G:" ++ gender_str)

  return (Character name gender)

tryParseChrFile :: String -> IO Character
tryParseChrFile filename = do
  contents <- readFile filename
  putStrLn contents
  return (Character "DefaultName" Male)
