# Unnamed chess game

A yet to be named chess engine with a nice terminal user interface. An UCI interface is also available. Watch it perform in [lichess](https://lichess.org/@/ThinkingPileORubbish).

## Highlights
- Supports Chess960
- Searches middle-game positions to a depth of 10 plies within a reasonable time, without trickeries such as depth reductions
- Takes advantage of up to 3 threads with *principal variation splitting*
- Basic piece-square table and mobility evaluation
- Evaluations are cached in a lock-free hash table
