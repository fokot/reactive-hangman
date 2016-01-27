# Hangman game

Simple hangman game.

The idea was to write this game in reactive style using takeWhile and scanl on the game state.
This is how they work with simple list of number

    Prelude> let l = [1, 2, 3, 4, 1, 1, 1, 1]
    Prelude> scanl (+) 0 l
    [0,1,3,6,10,11,12,13,14]
    Prelude> takeWhile (< 12) $ scanl (+) 0 l
    [0,1,3,6,10,11]

This is how it works in the game. [Streaming package](https://github.com/michaelt/streaming) is used, because otherwise I can't do lazy IO

	ioSteps :: GameState -> Stream (Of GameState) IO r
    ioSteps gs = S.iterateM updateGameStateIO (return gs)

    runGameInfiniteStream :: Monad m => Stream (Of GameState) m r -> Stream (Of GameState) m ()
    runGameInfiniteStream steps = S.take 1 $ S.dropWhile gameInProgress steps

Random words are get from this service "http://randomword.setgetgo.com/get.php"

This way I'm able to simulate the gameplay with providing guesses and pure functions
    
    pureSteps :: GameState -> [Char] -> Stream (Of GameState) Identity ()
    pureSteps gs chars = S.scan updateGameState gs id (S.each chars)

    pureGame :: GameState -> [Char] -> GameState
    pureGame = ...

    testCase "win game - not a straight guess" $
     pureGame car "cafr" @?= (GameState "car" 4 "rac")
