# Triolet

This is a simple simulation of the board game *Triolet*, written in Haskell.

![Game screenshot](screenshot.png)

## Basic rules

*Triolet* basically is "Scrabble with numbers".
The game revolves around on the three following simple constraints:

- at most 3 tiles can be aligned continuously;
- the sum of aligned tiles must not exceed 15;
- the sum of 3 aligned tiles must be equal to 15.

Completing alignments of 3 tiles is the main purpose since it doubles the points earned from the alignment.
You can find more information about the game at [BoardGameGeek](https://boardgamegeek.com/boardgame/13103/triolet).

## Status

Most of the game is implemented, except *joker* tiles (replace any tile).
Only the computer is playing, and it does so naively: it simply maximizes its points at each turn.

## Build and run

```
stack build --exec triolet
```

Press any key to advance the simulation.
