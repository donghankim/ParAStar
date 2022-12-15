{-

Problem 2: Word Ladder

Write a program that takes the name of a dictionary file and starting
and ending words and constructs the shortest "word ladder," i.e., a
sequence of dictionary words that differ by exactly one changed
character that transforms the starting word into the ending word.
Unlike some word ladder puzzles, your program should not consider
adding or removing letters.

Your program should read a Unix-style /usr/dict/words file (i.e., a
sorted, newline-separated list of valid words), and only consider
lowercase-only words (i.e., no capitalized proper nouns).  You may
also ignore words with punctuation, e.g., abase is fine, but ignore
aardvark's, ASCII, and Abby.

The user must supply a valid dictionary file and start and end words
that are the same length, all lowercase, and both in the dictionary.
Your program should print an error message and terminate with a
non-zero error code if supplied with erroneous arguments.

Search for ladders of at most 20 words, returning an error message if
one cannot be found with the given dictionary.  For a given pair of
words and dictionary, there is often more than one minimum-length
ladder; you may return any of them.

Here is an example run; your program should work with other
dictionaries, too.

$ wget https://raw.githubusercontent.com/eneko/data-repository/master/data/words.txt
$ stack --resolver lts-19.23 ghc -- --make -Wall -O wordLadder.hs
$ ./wordLadder words.txt fool sage
fool
foot
fort
fore
fare
fage
sage
$ ./wordLadder words.txt computer solution
Unable to find a ladder in 20
$ ./wordLadder
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo bar
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo bar baz
wordLadder: foo: openFile: does not exist (No such file or directory)
$ ./wordLadder words.txt fooa barb
"fooa" not in dictionary
$ ./wordLadder words.txt barb fooa
"fooa" not in dictionary
$ ./wordLadder words.txt bar none
"bar" and "none" must be the same length

As for the wordFreq problem, make your solution correct, readable, and
fast (in order of importance).  Again, we will classify the
performance of your solution into one of three categories:
substantially faster than our reference solution, about the same, and
substantially slower.

Our 68-line reference solution uses a BFS-style search implemented
with functions from System.IO, System.Exit, Data.Char,
System.Environment, Data.Set, Control.Monad, and Data.List.

For reference, the fool->sage example above took about 220 ms on my
desktop machine.  Our solution uses one fairly straightforward trick
to improve its performance, but is otherwise untuned.

-}

import Control.Monad.State
import Data.Char
import Data.List (find)
import Data.Set (Set, member, difference)
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.Exit ( die )


-- Not complete...
main :: IO ()
main = do
  (dict, from , to) <- getArgs >>= parseInput
  words <- lines <$> readFile dict
  case verifyInput words from to of
    1 -> die $ from ++ " and " ++ to ++ " must be the same length"
    2 -> die $ from ++ " not in dictionary"
    3 -> die $ to ++ " not in dictionary"
    4 -> print from
    _ -> print $ processDict words $ length from
  where
    parseInput [dict, from, to] = return (dict, from, to)
    parseInput _ = die "Usage: wordLadder <dictionary-filename> <from-word> <to-word>"

verifyInput:: [String] -> String -> String -> Int
verifyInput dict from to
  | length from /= length to = 1
  | from `notElem` dict = 2
  | to  `notElem` dict = 3
  | from == to = 4
  | otherwise = 5

processDict:: [String] -> Int -> [String]
processDict [] _ = []
processDict (str : xs) n = if all isLower str && all isAlpha str && length str == n
  then str:processDict xs n
  else processDict xs n


-- get all possible words from one change
getNextWords :: String -> Set String -> Set String
getNextWords word = Set.filter (oneHop word)
  where
    oneHop [] [] = False
    oneHop [] _ = False
    oneHop _ [] = False
    oneHop (x : xs) (y : ys)
      | x /= y = xs == ys
      | otherwise = oneHop xs ys


