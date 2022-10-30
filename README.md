# aoc-util


## Rationale

We want to be able to read the puzzle description, solve and submit it without leaving the repl.

For this we have to be able to download the description and the puzzle input. This library offers some convenience functions.

```clojure
(get!) ;;=> returns the input as a vector of strings, parsing the current namespace to infer the puzzle day

(get! "2020.7") ;;=> returns the input as a vector of strings

(get! "2020.7" str->int) ;;=> You can pass an optional function, defaults to the identity fn

(submit! 1 0) ;;=> Submit Puzzle answer part 1 answer 0, for the current namespace

(download-description) ;;=> Downloads the Puzzle description to resources/puzzle/{YEAR}/{DAY}.md 
```

## Docs

[https://pyons.github.io/aoc-util/](https://pyons.github.io/aoc-util/)


## Installation

deps.edn

```clojure
:deps
    {pyons/aoc-util {:git/url "https://github.com/pyons/aoc-util" :git/tag "v0.0.4" :git/sha "8afb2b6"}
```

## Examples

```clojure
(ns se.2020.day1
  (:require [aoc-util.util :refer [get! str->int download-description]]))

(def input (get!)) ;; gets the input for the puzzle year 2020 day 1
```

## License

Copyright 2021 Steffen

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
