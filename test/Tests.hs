module Main where

import Hangman (pureGame, GameState(..), newGameState)
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

--wonGameWithLives :: String -> [Char] -> () GameState
--wonGameWithLives word lives = let (GameState _ _ actualLives) = pureGame (GameState "car" [] 5) guess
--							  in actualLives @?= lives

car = GameState "car" 5 []

tests = testGroup "Hangman tests"
  [ testCase "win game - straight guess" $
     pureGame car "car" @?= (GameState "car" 5 "rac")
    ,
    testCase "win game - not a straight guess" $
     pureGame car "cafr" @?= (GameState "car" 4 "rac")
    ,
    testCase "win game - with one life" $
     pureGame car "fghtcar" @?= (GameState "car" 1 "rac")
    ,
    testCase "win game - reversed word" $
     pureGame car "rac" @?= (GameState "car" 5 "car")
    ,
    testCase "win game - stop after the win" $
     pureGame car "cafrgh" @?= (GameState "car" 4 "rac")
    ,
    testCase "loose game" $
     pureGame car "cognitive" @?= (GameState "car" 0 "c")
  ]