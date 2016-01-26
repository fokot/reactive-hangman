module Hangman where

import Control.Monad
import Data.List (find)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Streaming
import qualified Streaming.Prelude as S
import Control.Monad.Identity
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


ioSteps :: GameState -> Stream (Of GameState) IO r
ioSteps gs = S.iterateM updateGameStateIO (return gs)

runGameInfiniteStream :: Monad m => Stream (Of GameState) m r -> Stream (Of GameState) m ()
runGameInfiniteStream steps = S.take 1 $ S.dropWhile gameInProgress steps

pureSteps :: GameState -> [Char] -> Stream (Of GameState) Identity ()
pureSteps gs chars = S.scan updateGameState gs id (S.each chars)

pureGameTest = runGameInfiniteStream $ pureSteps (newGameState "car") "carfffff"


---------- Random word ----------
httpGet :: String -> IO String
httpGet url = simpleHTTP (getRequest url) >>= getResponseBody

randomWord = httpGet "http://randomword.setgetgo.com/get.php"

---------- Main ----------
main :: IO ()
main = do
  putStrLn "You are playing reactive Hangman!!!"
  word <- randomWord
  S.print $ runGameInfiniteStream $ ioSteps (newGameState word)


