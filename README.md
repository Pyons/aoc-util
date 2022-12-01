# aoc-util


## Rationale

We want to be able to read the puzzle description, solve and submit it without leaving the repl or editor.

For this we have to be able to download the description and the puzzle input. This library offers some helper functions.

The library is split into two namespaces, `aoc-utils.tools` which contains all functions you need to interact with advent of code.
And `aoc-utils.utils` which contains conviencece functions to parse and solve puzzles.

```clojure

(set-session-cookie! "33617c7465645f5..........................7701afc0")

(get!) ;;=> returns the puzzle input as a vector of strings, parsing the current namespace to infer the puzzle day

(get! 2022 1 identity) ;;=> returns the input as a vector of strings

(get! 2022 7 parse-long) ;;=> You can pass a function to be mapped over the lines

(get! parse-long) ;;=> returns the puzzle input as a vector of strings, mapping parse-long over each line

(submit-first! 0) ;;=> Submit Puzzle answer part 1 answer 0, for the current namespace

(submit-second! 0) ;;=> Submit Puzzle answer part 2 answer 0, for the current namespace

(download-description) ;;=> Downloads the Puzzle description to resources/puzzle/{YEAR}/{DAY}.md and converts it to markdown

(create-next-day) ;; => Creates a new file with your namespace and increasing the day value. e.g. se.2022.day1 -> se.2022.day2
```

## Session

![nvim session](https://github.com/Pyons/aoc-util/blob/master/day1.png?raw=true)

## Docs

[https://pyons.github.io/aoc-util/](https://pyons.github.io/aoc-util/)


## Installation

deps.edn

```clojure
:deps
    pyons/aoc-util {:git/url "https://github.com/pyons/aoc-util" :git/tag "v0.2.2" :git/sha "6010da5"}
```

## Example

```clojure
(ns se.2022.day1
  (:require [aoc-util.tools :refer
             [get!
              set-session-cookie!
              download-description
              submit-first!
              submit-second!
              create-next-day]]
            [aoc-util.utils :refer [split-by]]))

(def input (get!)) ;; gets the input for the puzzle year 2022 day 1

(comment
    (set-session-cookie! "5361762...01afc0")

    (submit-first! 0)
    (submit-second! 0)
 
    (create-next-day))
```

## License

Copyright 2021 Steffen

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
