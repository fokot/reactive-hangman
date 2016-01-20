# Hangman game

Simple hangman game.

The idea was to write this game in reactive style using takeWhile and scanl on the game state.
This is how they work with simple list of number

    Prelude> let l = [1, 2, 3, 4, 1, 1, 1, 1]
    Prelude> scanl (+) 0 l
    [0,1,3,6,10,11,12,13,14]
    Prelude> takeWhile (< 12) $ scanl (+) 0 l
    [0,1,3,6,10,11]

This is how it will work in the game

    takeWhile userWonTheGameOrHasNoMoreLives $ scanl updateStateWithUserGuess starState infiniteListOfUserGuesses
