# 2048.hs

This is a Haskell program to create an AI player for [2048][1]. Your algorithm should implement `getNextMove` (in `Algorithm.hs`), taking a grid of tiles and returning a move. You can and should use the functions in `Game.hs`. To run:

[1]: https://gabrielecirulli.github.io/2048/

```
make
./2048
```

## Testing Your Algorithm

I've included a simple test harness that runs the program 1000 times and returns the average number of turns and the sum of the tiles on the board. In general, more turns and a higher score is better. To run:

```
make test
```
