# advent-2019-clojure

This is my code for the 2019 [Advent of Code](https://adventofcode.com/2019), all solutions in [Clojure](https://clojure.org/).

All code is under the `src` directory. Each solution-file is named `dayNN.clj` and contains both puzzle solutions for that day. These are the publically-facing functions `p01` and `p02`. These files are the code *exactly as I used it to solve and submit the answers*. If I revisit any of the days and try to clean up or optimize the solutions, that work will be in a separate file that will be named `dayNNbis.clj`.

The `data` directory contains the input data for each day. These files are prefixed with the day number, i.e., `NN_something.txt` where `NN` is the day number and the rest is a (semi-)descriptive name. (There is no data file for day 4.)

## Usage

This project is managed with [Leiningen](https://leiningen.org/). Running the following will download any dependencies and start a REPL:

```
lein repl
```

## License

Copyright © 2019 Randy J. Ray

Distributed under the Eclipse Public License either version 2.0 or (at your option) any later version.
