<a href='http://www.recurse.com' title='Made with love at the Recurse Center'><img src='https://cloud.githubusercontent.com/assets/2883345/11322972/9e553260-910b-11e5-8de9-a5bf00c352ef.png' height='59px'/></a>

# Hangman game

Simple hangman game.

The idea was to write this game in reactive style using takeWhile and scanl on the game state.
This is how they work with simple list of number

    Prelude> let l = [1, 2, 3, 4, 1, 1, 1, 1]
    Prelude> scanl (+) 0 l
    [0,1,3,6,10,11,12,13,14]
    Prelude> takeWhile (< 12) $ scanl (+) 0 l
    [0,1,3,6,10,11]

IO is not lazy so it can be done only with use of unsafeInterleaveIO to make IO lazy

    runGameM :: Monad m => GameState -> (GameState -> m [GameState]) -> m GameState
    runGameM gs stream = do 
      steps <- stream gs
      return $ head $ dropWhile gameInProgress steps

    gsIO :: GameState -> IO [GameState]
    gsIO gs = do
      l <- getALetter gs
      let ngs = updateGameState gs l 
      g <- unsafeInterleaveIO $ gsIO ngs
      return $ (ngs : g)

    runInteractiveGame = runGameM (newGameState "car") gsIO

And this is how game can be tested
    
    gsPure :: [Char] -> GameState -> Identity [GameState]
    gsPure (c:cs) gs = do 
      let newGs = updateGameState gs c
      nextGs <- gsPure cs newGs
      return (newGs : nextGs)

    runPureGame = runGameM (newGameState "car") $ gsPure "cfar"

Or istead of using unsafeInterleaveIO I also made a version with [streaming package](https://github.com/michaelt/streaming) 

    updateGameStateIO :: GameState -> IO GameState
    updateGameStateIO = ...
    
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
