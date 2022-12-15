{-

 Name: Donghan Kim
 Uni: dk3245

 Collaborators: N/A

 References:
 https://www.youtube.com/watch?v=Vgu82wiiZ90&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
 ------------------------------

 COMS 4995 002 Parallel Function Programming

 Homework 1

 Due at 11:59 PM Sunday, September 25, 2022

 Modify this file with your solutions and submit it on Courseworks

 You may use functions, etc. from the Standard Prelude, but no
 other libraries

 Do not modify the type signatures for any of the provided functions.

 Above, include your name, UNI, list of people with whom your spoke about the
 assignment, and online references your consulted.

 Write your code alone.  You many consult the instructor, TAs, and other
 students, but do not copy/modify other's code.

 Please do not delete or modify any of the block comments below (i.e.,
 {- -} comments) as we use them to identify where your solutions begin and end.

 Feel free to add and delete single-line comments (i.e., --)

 Put any helper functions you add for a particular problem between
 the comment for the problem and the comment for the next problem

 -----

 Grading: 70% correctness: first and foremost, it needs to be correct
          30% style: is it readable, functional, concise?

 Use lts-19.23 as the "resolver" for the Haskell Tool Stack.
 E.g., stack --resolver lts-19.23 ghci

 Your code should load under GHCi 9.0.2 with no warnings under -Wall, e.g.
 :set -Wall
 :l hw1

-}

{- 1) Write a function elem' x xs that returns true if x is an element of the
      list xs.  Assume the list xs is in increasing order but may be infinite.

      E.g,

      *Main> elem' (1::Int) []
      False
      *Main> elem' (57::Int) [1..]
      True
      *Main> elem' (57::Int) [1,3..]
      True
      *Main> elem' (57::Int) [1..100]
      True
      *Main> elem' (58::Int) [1,3..]
      False

-}
elem' :: Ord a => a -> [a] -> Bool
elem' _ [] = False
elem' val (x:xs) = (val == x) || elem val xs

{- 2) Write a function for testing the Goldbach conjecture, i.e., that
      every even integer greater than 2 can be written as a sum of two
      primes.  Write a function goldbach n that returns a pair of
      primes (p1, p2) such that p1 + p2 = n.  Use the list primes and
      the elem' function in your solution.  Assume n is in [4,6,...].
      Avoid a O(n^3) search.  Note: the solution often isn't unique;
      you may return any pair of primes that sum to n.

      Hint: my solution was mostly a one-line list comprehension.
      Generate candidate solutions and filter them.

      Example:

      *Main> take 10 [ goldbach n | n <- [4,6..] ]
      [(2,2),(3,3),(3,5),(3,7),(5,7),(3,11),(3,13),(5,13),(3,17),(3,19)]
      *Main> goldbach 2642
      (103,2539)
      *Main> goldbach 65536
      (17,65519)

-}

-- An infinite list of primes; do not modify
primes :: [Integer]
primes = f [2..] where f (p:xs) = p : f [ x | x <- xs, x `mod` p /= 0 ]
                       f [] = []

goldbach :: Integer -> (Integer, Integer)
-- goldbach n = (n,n) -- Change this
goldbach n = head [(p1,p2) | p1 <- [1..n-1],
                             p2 <- [p1+1..n],
                             elem' p1 (take 1000 primes) && elem' p2 (take 1000 primes),
                             p1 + p2 == n]

{- 3) Write a function "maxrun" that reports the length of the longest
      contiguous run of equal values in a list.

      E.g.,

      *Main> maxrun ([] :: [Int])
      0
      *Main> maxrun [1 :: Int]
      1
      *Main> maxrun [1 :: Int,1]
      2
      *Main> maxrun "aabbbcdd"
      3
-}

maxrun :: Eq a => [a] -> Int
maxrun [] = 0
maxrun (x:xs) = if x == head xs then 1 + maxrun xs
                else maxrun xs


{- 4) Write the infinite list of Pell numbers in which the next number is
      twice the previous number plus the the number before that.

      Hint: declare the list recursively in terms of itself.  My solution is
      a single line.

      Example:

      *Main> take 10 pell
      [0,1,2,5,12,29,70,169,408,985]
-}

pell :: [Integer]
pell = [1,2,3]

{- 5) Use the list of Pell numbers to construct a list of successive
      approximations to the square root of 2, that
      is, a compute a list derived from the Pell numbers whose entries are

      P    + P
       n-1    n
     -----------
          P
           n

     and verify that it approaches the square root of 2: 1.4142135623730951

     Hint: I converted the Integer Pell numbers using fromIntegral.
     My solution was two lines

     Example:
     *Main> take 6 sqrt2
     [1.0,1.5,1.4,1.4166666666666667,1.4137931034482758,1.4142857142857144]

-}

sqrt2 :: [Double]
sqrt2 = [] -- Change this

{- 6) Write a digsum function that reports the sum of the digits of
      an integer.  Negative inputs should report a negative sum.
      Use the quotRem function, which is defined for the Integral type class,
      to pick apart the number into digits.

      Do better than the solutions on Stack Overflow (most of which use
      non-Prelude functions, anyway):

      https://stackoverflow.com/questions/2838727/how-do-i-get-the-sums-of-the-digits-of-a-large-number-in-haskell

      Examples:

      *Main> digsum (0 :: Int)
      0
      *Main> digsum (123 :: Int)
      6
      *Main> digsum ((-123) :: Int)
      -6
      *Main> digsum (123456789 :: Integer)
      45
      *Main> digsum (100010000 :: Integer)
      2

-}
digsum :: Integral a => a -> a
digsum _ = 0 -- Change this

{- 7) Generate the rows of Pascal's triangle as an infinite list of lists.

      Hint: use zipWith, tail, and list comprenensions.  As an intermediate
      step, try writing a function that takes one line of the triangle
      and produces the next.  My final solution is a single line.

    Example:
    *Main> take 7 (pascal :: [[Int]])
    [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1],[1,6,15,20,15,6,1]]

-}
pascal :: Num a => [[a]]
pascal = [] -- Change this
