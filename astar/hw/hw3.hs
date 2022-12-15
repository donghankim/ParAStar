import Data.Word
import Data.Maybe

-- You may import other libraries for this assignment; do so below this line

{-

 Name: Donghan Kim
 Uni: dk3245

 Collaborators:

 References:

 ------------------------------

 COMS 4995 002 Parallel Function Programming

 Homework 3

 Due at 11:59 PM Sunday, October 23, 2022

 Modify this file with your solutions and submit it on Courseworks

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

 Since this homework is a bit short, its overall class weight will be
 lower than others.

 Use lts-19.23 as the "resolver" for the Haskell Tool Stack.
 E.g., stack --resolver lts-19.23 ghci

 Your code should load under GHCi 9.0.2 with no warnings under -Wall, e.g.
 :set -Wall
 :l hw3

 ==============================

 In the first nine problems of this assignment, you will implement an
 infinite precision integer arithemtic class called "Int100" that uses
 lists of Word8 (unsigned bytes) to represent integers in base 100 (to
 simplify decimal printing)

 Each list of Word8 digits consists of at least one digit and the last
 element -- the most significant digit -- must be non-zero.

 E.g.,

 IntP [0,2] represents 200
 IntN [5]   represents -5
 IntZ       represents 0
 IntP []    is illegal (must have at least one digit)
 IntN [0]   is illegal (last element most be non-zero)
 IntP [1,0] is illegal (last element must be non-zero)
 IntP [100] is illegal (digits must be in [0..99])

-}

data Int100 = IntP [Word8]
            | IntN [Word8]
            | IntZ
    deriving Eq

{- 1) Write the toInt100 function, which converts an
      Integer to an Int100

      E.g.,

      toInt100 0 = IntZ
      toInt100 (-1) = IntN [1]
      toInt100 99 = IntP [99]
      toInt100 100 = IntP [0,1]
      toInt100 (-199) = IntN [99,1]

      This is the only problem in which you are allowed to use
      functions that operate on the Integer type.
 -}

toWord8 :: Integer -> Word8
toWord8 = fromIntegral

toInt100 :: Integer -> Int100
toInt100 0 = IntZ


{- 2) Write an instance of Show (just the show function)
      for the Int100 type.  You may assume the Int100 object
      is not illegal, i.e., every digit in the lists is in the range [0..99]
      and every list ends in a non-zero digit.

      The implementation provided below is intended for you to use
      while you are debugging other functions

      E.g.,

      show IntZ         = "0"
      show (IntP [3,4]) = "403"
      show (IntN [42])  = "-42

      You may not use any existing Integer code in your solution.
-}

instance Show Int100 where
  show IntZ = "0"
  show (IntP []) = ""
  show (IntN []) = ""
  show (IntP (x : xs)) = show (x) ++ show (IntP xs)

-- consindering using foldr to convert first into Num
tempP = IntP [3,4]
arr2 = IntN [1,2,3,4]




{- 3) Write a function addDigits that adds two positive integers
      represented as lists in base-100 form, LSD first (e.g., as in an Int100).
      Assume every list element is in [0..99] and that the last element, if
      any, is non-zero.

      Your function should produce a list whose last element is non-zero
      or the empty list.

      E.g.,

      addDigits [] [] = []
      addDigits [] [1,2,3]  = [1,2,3]
      addDigits [99,1] [1]  = [0,2]
      addDigits [99,99] [1] = [0,0,1]
      addDigits [99,99,99] [99,99] = [98,99,0,1]

      You may not use any existing Integer code in your solution.
-}
addDigits :: [Word8] -> [Word8] -> [Word8]
addDigits _ _ = [] -- Change this

{- 4) Write a function subDigits that subtracts the second positive
      integer from the first.  Assume both numbers are in base-100 form,
      the last element, if any, is non-zero, and that the
      first number is greater or equal to the second (i.e., that the result
      is non-negative)

      Your function should produce a list in base-100 form, i.e.,
      each list element is in [0..99] and the last list element, if any,
      is non-zero.

      E.g.,

      subDigits [] [] = []
      subDigits [1,2,3,4] [1,2,3,4] = []
      subDigits [1,2,3,4] [0,2,3,4] = [1]
      subDigits [0,0,0,0,1] [1] = [99,99,99,99]
      subDigits [0,0,0,1] [0,1] = [0,99,99]
      subDigits [99,99,0,99] [1,10,1,1] = [98,89,99,97]

      You may not use any existing Integer code in your solution.
-}
subDigits :: [Word8] -> [Word8] -> [Word8]
subDigits _ _ = [] -- Change this

{- 5) Write a function multDigit that multiplies a positive base-100 integer
      by an integer in the range [0..99].  Assume the last element of the
      base-100 list is non-zero and that the input integer is in the range
      [0..99]

      E.g.,

      multDigit 0 [42] = []
      multDigit 1 [1,2,3] = [1,2,3]
      multDigit 2 [1,2,3] = [2,4,6]
      multDigit 99 [99,99,99] = [1,99,99,98]

      You may not use any existing Integer code in your solution.
-}
multDigit :: Word8 -> [Word8] -> [Word8]
multDigit _ _ = [] -- Change this

{- 6) Write a function multDigits that multiplies two positive base-100
      integers.  You may use multDigit and addDigits.

      E.g.,

      multDigits [] [1,2,3] = []
      multDigits [1,2,3] [] = []
      multDigits [1] [99,98,97] = [99,98,97]
      multDigits [0,1] [99,98,97] = [0,99,98,97]
      multDigits [0,2] [99,98,97] = [0,98,97,95,1]
      multDigits [99,99,99] [99,99,99] = [1,0,0,98,99,99]

      You may not use any existing Integer code in your solution.
-}
multDigits :: [Word8] -> [Word8] -> [Word8]
multDigits _ _ = [] -- Change this

{- 7) Write a function compare100 that compares the magnitude of the
      two base-100 numbers given to it.
      Assume both input lists have non-zero last elements, but
      that they may be of different lengths.

      E.g.,

      compare100 [] [] = EQ
      compare100 [1] [] = GT
      compare100 [] [1] = LT
      compare100 [55,1] [55,1] = EQ
      compare100 [0,56] [1,55] = GT
      compare100 [0,0,0,2] [99,99,99,1] = GT
      compare100 [98,99,5] [99,98,5] = GT
      compare100 [99,98,5] [98,99,5] = LT
-}
compare100 :: [Word8] -> [Word8] -> Ordering
compare100 _ _ = EQ

{- 8) Write an instance of the Num type class for Int100.
      Include implementations of fromInteger, +, *, signum, abs,
      and negate. Use your toInt100, addDigits, subDigits, multDigits,
      and compare100 functions.

      E.g.,

     (fromInteger (-1234567)) :: Int100 = -1234567
     fromInteger 0 :: Int100 = 0
     fromInteger 42 :: Int100 = 42

     signum ((-5) :: Int100) = -1
     signum (0 :: Int100) = 0
     signum (421 :: Int100) = 1

     negate ((-123) :: Int100) = 123
     negate (0 :: Int100) = 0
     negate (453 :: Int100) = -453

     abs ((-123) :: Int100) = 123
     abs (123 :: Int100) = 123
     abs (0 :: Int100) = 0

     1234 + (4567 :: Int100) = 5801
     1234 + ((-1233) :: Int100) = 1
     1232 + ((-1233) :: Int100) = -1
     1234 - (1234 :: Int100) = 0

     99 * ((-99) :: Int100) = -9801
     (-998801) * (200 :: Int100) = -199760200
     (-16) * ((-32) :: Int100) = 512

     You may not use any existing Integer code in your solution.
-}
instance Num Int100 where
   fromInteger = toInt100
   _ + _ = IntZ -- Change this
   _ * _ = IntZ -- Change this
   signum _ = IntZ -- Change this
   negate _ = IntZ -- Change this
   abs _ = IntZ -- Change this

{- 9) Write an instance of the Ord type class for Int100.
      in particular, write an implementation of the compare
      function; Haskell will infer the rest.  Use your
      compare100 function.

      E.g.,

     (0 :: Int100) `compare` 0 = EQ
     (0 :: Int100) `compare` 1 = LT
     (1 :: Int100) `compare` 0 = GT
     (1 :: Int100) `compare` 1 = EQ
     ((-1) :: Int100) `compare` 1 = LT
     (1 :: Int100) `compare` (-1) = GT

     You may not use any existing Integer code in your solution.
-}
instance Ord Int100 where
  _ `compare` _ = EQ -- Change this

{- 10) Implement the Functor type classes for the BHeap and BTree types.

     Your solution should satisfy the functor properties:
     fmap id = id
     fmap ( f . g ) = fmap f . fmap g
-}
data BHeap a = BHeap [BTree a] deriving (Eq, Show, Read)
data BTree a = BNode Int a (BHeap a) deriving (Eq, Show, Read)

instance Functor BHeap where
  fmap _ _ = error "" -- Change this

instance Functor BTree where
  fmap _ _ = error "" -- Change this
