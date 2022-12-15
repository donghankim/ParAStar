module Submission where
import System.IO
import Control.Monad(MonadPlus, guard)
import Data.List(intercalate)
{-

 Name: Donghan Kim
 Uni: dk3245

 Collaborators:

 References:

 ------------------------------

 COMS 4995 002 Parallel Functional Programming

 Homework 5

 Due at 11:59 PM Sunday, November 20, 2022

 Modify this file with your solutions and submit it on Courseworks

 Do not modify the type signatures for any of the provided functions

 Above, include your name, UNI, list of people with whom your spoke about the
 assignment, and online references your consulted.

 Write your code alone.  You many consult the instructor, TAs, and other
 students, but do not copy/modify other's code.

 Please do not delete or modify any of the block comments below (i.e.,
 {- -} comments) as we use them to identify where your solutions begin and end.

 Feel free to add and delete single-line comments (i.e., --)

 -----

 Grading: 70% correctness: first and foremost, it needs to be correct
          30% style: is it readable, functional, concise?

 Use lts-19.23 as the "resolver" for the Haskell Tool Stack.

 Your code should load under GHCi 9.0.2 with no warnings under -Wall, e.g.,

 stack --resolver lts-19.23 ghci
 :set -Wall
 :l hw5

-}

data Tree a = Branch (Tree a) (Tree a) | Leaf a
  deriving (Eq, Show)

{- 1) Write the toList function for the Tree type.  Leaves should
      appear in order from left to right in the result, e.g.,

  toList (Leaf 1) = [1]
  toList (Branch (Leaf 1) (Leaf 2)) = [1,2]
  toList (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)) = [1,2,3]

-}
toList :: Tree a -> [a]
toList (Leaf a) = [a]
toList (Branch a b) = toList a ++ toList b

{- 2) Write a fromList function for the Tree type.  Have it generate
      a nearly balanced tree, e.g.,

   fromList [1] = Leaf 1
   fromList [1,2] = Branch (Leaf 1) (Leaf 2)
   fromList [1..5] = Branch (Branch (Leaf 1) (Leaf 2))
                            (Branch (Leaf 3) (Branch (Leaf 4) (Leaf 5)))

   Have fromList [] throw an exception

   Make sure toList . fromList = id for everything but the empty list
-}
fromList :: [a] -> Tree a
fromList [] = error "Cannot create a Tree with an empty list..."
fromList [x] = Leaf x
fromList xs = Branch (fromList left) (fromList right)
            where
              half = div (length xs) 2
              left = take half xs
              right = drop half xs

{- 3) Make Tree and instance of the Functor class by implementing fmap, e.g.,

   toList $ (+100) <$> fromList [1..7] = [101..107]
-}
instance Functor Tree where
 -- fmap (a->b) -> Tree a -> Tree b
 fmap func (Leaf a) = Leaf (func a)
 fmap func (Branch a b) = Branch (fmap func a) (fmap func b)

{- 4) Make Tree an instance of the Applicative class by implementing pure
      and the <*> operator.  Use the "all combinations" semantics for
      the <*> operator.  E.g.,

    toList $ pure (+100) <*> fromList [1..5] = [101..105]

    toList $ fromList [(+),(*)] <*> fromList [1..3] <*> fromList [10,100,1000] =
    [11,101,1001,12,102,1002,13,103,1003,10,100,1000,20,200,2000,30,300,3000]

    Note that the <*> operator need not produce balanced trees
-}
instance Applicative Tree where
  pure x = Leaf x
  -- (<*>) :: Tree (a ->b) -> Tree a -> Tree b
  Leaf func <*> a = fmap func a
  (Branch fa fb) <*> t = Branch (fa <*> t) (fb <*> t)

{-
  Monads, and in particular the List being used as a MonadPlus, can be
  used for performing database-like query operations such as joins.

  In the problems below, you will implement some database queries
  using the MonadPlus typeclass and see how Monadic code can be conveniently
  used with different Monads.

  I adapted the following tables from

  Takahashi and Azuma, The Manga Guide to Databases, No Starch Press, 2009

  which is an amusing yet informative introduction to relational databases
  and SQL.
-}

-- A Product has an ID, a name, and a price
data Product = Product { pProdID :: Int
                       , pName :: String
                       , pPrice :: Int } deriving Eq

-- A Customer has an ID and a name
data Customer = Customer { cCustID :: Int
                         , cName :: String } deriving Eq

-- A Sale is a transaction on a particular day with a particular customer
data Sale = Sale { sSaleID :: Int
                 , sDate :: String
                 , sCustID :: Int } deriving Eq

-- An Item is part of a particular sale of a particular quantity of a product
data Item = Item { iSaleID :: Int
                 , iProdID :: Int
                 , iQuantity :: Int } deriving Eq

-- Show instances for the various table rows

printFields :: [a -> String] -> a -> String
printFields functs object = intercalate " " $ map ($ object) functs

instance Show Product where
  show = printFields [show . pProdID, pName, show . pPrice]

instance Show Customer where
  show = printFields [show . cCustID, cName]

instance Show Sale where
  show = printFields [show . sSaleID, sDate, show . sCustID]

instance Show Item where
  show = printFields $ map (show.) [iSaleID, iProdID, iQuantity]

{- 5) Write the getProduct, getCustomer, getSale, and getItem "functions"
      for the IO Monad that use hGetLine to read a line of input from
      the given handle, split it into space-separated words with the
      Standard Prelude words function, and the read function to convert
      strings to Ints as necessary

      E.g., if you type a well-formatted row of each type, it will appear
      to simply echo it back, but in fact it is using the "show" instances
      for each row type.

ghci> import System.IO(stdin)
ghci> getProduct stdin
103 Strawberry 53
103 Strawberry 53
ghci> getCustomer stdin
46 Stephen
46 Stephen
ghci> getSale stdin
103 5/7/2011 53
103 5/7/2011 53
ghci> getItem stdin
32 57 102
32 57 102

      Assume each input line is syntactially correct (e.g., names do
      not contain spaces); you may throw an exception on malformed input.

      Use system.IO.hGetLine to read a line from a Handle
-}

getProduct :: Handle -> IO Product
getProduct fp = do
  line <- hGetLine fp
  let tokens = words line
  let newProd = Product {
    pProdID =  read (tokens !! 0)::Int,
    pName = tokens !! 1,
    pPrice = read (tokens !! 2)::Int
  }
  return newProd

getCustomer :: Handle -> IO Customer
getCustomer fp = do
  line <- hGetLine fp
  let tokens = words line
  let newCustomer = Customer {
    cCustID = read (tokens !! 0)::Int,
    cName = tokens !! 1
  }
  return newCustomer

getSale :: Handle -> IO Sale
getSale fp = do
  line <- hGetLine fp
  let tokens = words line
  let newSale = Sale {
    sSaleID = read (tokens !! 0)::Int,
    sDate = tokens !! 1,
    sCustID = read (tokens!! 2)::Int
  }
  return newSale

getItem :: Handle -> IO Item
getItem fp = do
  line <- hGetLine fp
  let tokens = words line
  let newItem = Item {
    iSaleID = read (tokens !! 0)::Int,
    iProdID = read (tokens !! 1)::Int,
    iQuantity = read (tokens !! 2)::Int
  }
  return newItem

{- 6) Write the readTableFile function that, given a filename and
      one of your get functions, reads the file to produce a list of
      rows.

      Use System.IO.hIsEOF to detect when you've reached the end of input,
      and System.IO.withFile with System.IO.ReadMode to read a file into
      a Handle

      E.g., make sure it works on the text table files provided:

ghci> readProdTable >>= mapM_ print
101 Melon 800
102 Strawberry 150
103 Apple 120
104 Lemon 200
201 Chestnut 100
202 Persimmon 160
301 Peach 130
302 Kiwi 200
ghci> readCustTable >>= mapM_ print
12 The_Kingdon_of_Minanmi
23 Alpha_Empire
25 The_Kingdon_of_Ritol
ghci> readSaleTable >>= mapM_ print
1101 3/5 12
1102 3/8 23
1103 3/8 25
1104 3/10 12
1105 3/12 25
ghci> readItemTable >>= mapM_ print
1101 101 1100
1101 102 300
1102 101 100
1102 103 1700
1103 104 500
1104 201 2500
1105 301 2000
1105 104 700

-}

processHandle :: Handle -> (Handle -> IO a) -> IO [a]
processHandle fp func = do
  done <- hIsEOF fp
  if done
    then return []
    else do
      info <- func fp
      _ <- processHandle fp func
      return [info]


readTableFile :: String -> (Handle -> IO a) -> IO [a]
readTableFile fname func = withFile fname ReadMode (\fp -> do
  processHandle fp func)

-- These helper functions help to read in the various tables;
-- please do not modify them

readProdTable :: IO [Product]
readProdTable = readTableFile "products.txt" getProduct

readCustTable :: IO [Customer]
readCustTable = readTableFile "customers.txt" getCustomer

readSaleTable :: IO [Sale]
readSaleTable = readTableFile "sales.txt" getSale

readItemTable :: IO [Item]
readItemTable = readTableFile "items.txt" getItem



{- 7) Write selectName, a selector function that extracts the product name
      from a product row in a Monad, e.g.,

ghci> selectName $ Just $ Product 10 "Melon" 42
Just "Melon"

ghci> readProdTable >>= \t -> mapM_ putStrLn (selectName t)
Melon
Strawberry
Apple
Lemon
Chestnut
Persimmon
Peach
Kiwi

  Use a do block, >>=, or one of the applicative operators.

  Note that by writing this in the Monadic style, selectName operates
  just as well on "Maybe" rows as a table of rows in a list

-}

selectName :: Monad m => m Product -> m String
-- selectName = error "replace with your own solution"
selectName mProd = do
  prod <- mProd
  return $ pName prod


{- 8) Write custByDate, a join function that returns a date/customer name
      pair if a Sale row and a Customer row have the same customer ID.

      Use Control.Monad.guard to filter out non-matching rows.

      E.g.,

ghci> custByDate (Just $ Sale 1101 "3/5" 12) (Just $ Customer 12 "Stephen")
Just ("3/5","Stephen")

ghci> custByDate (Just $ Sale 1101 "3/5" 12) (Just $ Customer 11 "Stephen")
Nothing

ghci> salesTable <- readSaleTable
ghci> custTable <- readCustTable
ghci> mapM_ print $ custByDate salesTable custTable
("3/5","The_Kingdon_of_Minanmi")
("3/8","Alpha_Empire")
("3/8","The_Kingdon_of_Ritol")
("3/10","The_Kingdon_of_Minanmi")
("3/12","The_Kingdon_of_Ritol")

-}


custByDate :: MonadPlus m => m Sale -> m Customer -> m (String, String)
custByDate mSale mCust = do
  sale <- mSale
  customer <- mCust
  guard (sCustID sale == cCustID customer)
  return (sDate sale, cName customer)


{- 9) Write billsOnDate, a multi-way join that returns the amount of
      money charged to each customer for each product sold in a given date.

      That is, for all the sales on a given date, return a triple
      consisting of the name of the customer for the sale, the name of the
      product being sold, and its total price (unit * quantity)

      E.g.,

ghci> saleTable <- readSaleTable
ghci> custTable <- readCustTable
ghci> itemTable <- readItemTable
ghci> prodTable <- readProdTable
ghci> mapM_ print $ billsOnDate saleTable custTable itemTable prodTable "3/8"
("Alpha_Empire","Melon",80000)
("Alpha_Empire","Apple",204000)
("The_Kingdon_of_Ritol","Lemon",100000)

-}

billsOnDate :: MonadPlus m
               => m Sale -> m Customer -> m Item -> m Product -> String
               -> m (String, String, Int)
billsOnDate mSale mCust mItem mProd datePref = do
  temp <- custByDate mSale mCust
  guard ((fst temp) == datePref)
  prod <- mProd
  item <- mItem
  let tp = pPrice prod * iQuantity item
  return (snd temp, pName prod, tp)







