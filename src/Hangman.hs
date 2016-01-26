{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Hangman where

import Control.Monad
import Data.List (find)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Streaming
import qualified Streaming.Prelude as S
--import Control.Monad.State
--import Control.Monad.Identity
import Network.HTTP

data GameState = GameState {
  secretWord :: String,
  lives :: Int,
  guesses :: [Char]
} deriving (Eq, Show)

newGameState word = GameState (map toLower word) 5 []

showWord state = map (\c -> if elem c (guesses state) then c else '_') (secretWord state)
showState s = "You have " ++ (show $ lives s) ++ " lifes. The word is \"" ++ (showWord s) ++ "\""

updateGameState :: GameState -> Char -> GameState
updateGameState s c = if elem c (secretWord s) 
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
runGameTesting :: GameState -> [Char] -> GameState
runGameTesting gs stream = head $ dropWhile gameInProgress $ scanl updateGameState gs stream


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
    runGameRecursively $ updateGameState gs l

---------- Infitine list version of game ----------
-- runGameIntinite (newGameState "secret")
updateGameStateIO :: GameState -> IO GameState
updateGameStateIO gs = do
   l <- getALetter gs
   return $ updateGameState gs l

--ioStream :: Stream (S.Of GameState) IO ()
--ioStream = S.iterateM updateGameState (return gs)

runGameInfiniteStream ::  Monad m => (GameState -> m GameState) -> GameState -> Stream (S.Of GameState) m ()
runGameInfiniteStream updateGameStateFunction gs = S.take 1 $ S.dropWhile gameInProgress steps
   where
   --steps :: Monad m => GameState -> (GameState -> m GameState) -> Stream (S.Of GameState) m ()
   steps = S.iterateM updateGameStateFunction (return gs)

---------- Main ----------
main :: IO ()
-- main = putStrLn ""
main = do
  word <- randomWord
  S.print $ runGameInfiniteStream updateGameStateIO (newGameState word)

--s :: [Char] -> GameState -> State [Char] GameState
--s = undefined

----m = runGameInfiniteStream (updateGameStateTest "asdafsdsa") (newGameState "car")

--updateGameStateTest :: MonadState [Char] m => [Char] -> GameState -> m GameState
--updateGameStateTest = undefined
--updateGameStateTest gs = 
--  (do (l:ls) <- get
--      put ls
--      return (updateGameState gs l)
--  )

--updateGameStateTest2 :: [Char] -> GameState -> Identity GameState
--updateGameStateTest2 (x:xs) gs = return $ updateGameState gs x

--mainn = S.print $ runGameInfiniteStream (updateGameStateTest "") (newGameState "car")

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

randomWord = get "http://randomword.setgetgo.com/get.php"

type Stack = [Int]  
  
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  
  
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2  

