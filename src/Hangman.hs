{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Hangman where

import Control.Monad
import Data.List (find)
import Data.Char (toLower)
import Data.Maybe (fromJust)
-- import Debug.Trace

data GameState = GameState {
  secretWord :: String,
  lives :: Int,
  guesses :: [Char]
} deriving (Eq, Show)

newGameState word = GameState (map toLower word) 5 []

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


---------- Version of game to test game logic. Pass [Chars] as user input ----------
-- runGameTesting (newGameState "secret") "secrfta"
runGameTesting :: GameState -> [Char] -> [GameState]
runGameTesting gs stream = takeWhile gameInProgress $ scanl updateState gs stream


-- gets a letter from terminal
getALetter :: GameState -> IO Char
getALetter gs = do
  putStrLn $ showState gs
  putStrLn "Guess a letter: "
  fmap (toLower . head) getLine

---------- Recursive version of game ----------
-- runGameRecursively (newGameState "secret")

runGameRecursively :: GameState -> IO ()
runGameRecursively gs =
  if gameEnded gs
  then putStrLn $ showState gs
  else do
    l <- getALetter gs
    runGameRecursively $ updateState gs l

---------- Infitine list version of game ----------
-- runGameIntinite (newGameState "secret")
updateGameState :: GameState -> IO GameState
updateGameState gs = do
   l <- getALetter gs
   return $ updateState gs l

ff :: (a -> Bool) -> [IO a] -> IO a
ff f (i:is) = do
  res <- i
  if f res then return res else ff f is

fff :: (a -> Bool) -> [IO a] -> IO a
fff f (i:is) = do
  res <- i
  if f res then return res else ff f is

iii !x = x >>= updateGameState

runGameInfinite :: GameState -> IO ()
runGameInfinite gs =
  -- infinite lazy game loop
  let repl = tail $ iterate (\x -> x >>= updateGameState) (return gs) :: [IO GameState]
  in do
    endState <- fff gameEnded repl
--     endState <- last $ take 3 repl
--     endState <- fmap fromJust $ liftM (find gameEnded) (sequence repl)
    putStrLn $ showState endState

---------- Main ----------
main :: IO ()
main = runGameInfinite (newGameState "car")


