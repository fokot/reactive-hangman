{-# LANGUAGE FlexibleInstances #-}
module Hangman where

import Control.Monad

data GameState = GameState {
  secretWord :: String,
  guesses :: [Char],
  lives :: Int
} deriving (Eq, Show)

showWord state = map (\c -> if elem c (guesses state) then c else '_') (secretWord state)
showState s = "You have " ++ (show $ lives s) ++ " lifes. The word is \"" ++ (showWord s) ++ "\""

updateState :: GameState -> Char -> GameState
updateState s c = if elem c (secretWord s) 
  then s {guesses = c : (guesses s)}
  else s {lives = (lives s) - 1}

-- guessedWord s = all (\x -> elem x (guesses s)) (secretWord s)
guessedWord s = all (flip elem $ guesses s) $ secretWord s

combine :: (b -> b -> b) -> (a -> b) -> (a -> b)  -> a -> b
combine op f1 f2 a = op (f1 a) (f2 a)

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) = combine (||)

noLives s = (lives s) <= 0
gameEnded = noLives ||| guessedWord
gameInProgress = not . gameEnded


class CharacterStream a where
  getCharacters :: a -> [Char]

instance CharacterStream [Char] where
  getCharacters = id


runGame :: (CharacterStream s) => String -> Int -> s -> [GameState]
runGame word lives stream = takeWhile gameInProgress $ scanl updateState startState (getCharacters stream)
  where startState = GameState word [] lives


doStep :: GameState -> IO ()
doStep gamestate =
  if gameEnded gamestate
  then putStrLn "End of the game. You either won or lost ;)"
  else do
    putStrLn $ showState gamestate
    putStrLn "Guess next letter"
    c <- getLine
    doStep $ updateState gamestate (head c)

main :: IO ()
main = do
  putStrLn "Hangman is starting"
  doStep $ GameState "fero" [] 5


-- runGame2 :: (CharacterStream s) => String -> Int -> s -> [(GameState, IO Char)]
-- runGame2 word lives stream = takeWhile gameInProgress $ scanl updateState startState (getCharacters stream)
--   where startState = GameState word [] lives