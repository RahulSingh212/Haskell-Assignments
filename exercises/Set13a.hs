{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-} -- this silences an uninteresting warning

module Set13a where

import Mooc.Todo

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Examples.Bank

------------------------------------------------------------------------------
-- Ex 1: Your task is to help implement the function readName that
-- given a string like "Forename Surname" produces the pair
-- ("Forename", "Surname"). readName should fail (return Nothing) in
-- the following cases:
--
--   1. the input string doesn't contain a space
--   2. one of the names contains numbers
--   3. one of the names doesn't start with a capital letter
--
-- The function readNames has already been implemented using the ?>
-- operator from the course material. You need to define the helper
-- functions split, checkNumber and checkCapitals so that readNames
-- works correctly.

(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing   -- In case of failure, propagate failure
Just x  ?> f = f x       -- In case of success, run the next computation

-- DO NOT touch this definition!
readNames :: String -> Maybe (String,String)
readNames s =
  split s
  ?>
  checkNumber
  ?>
  checkCapitals

-- split should split a string into two words. If the input doesn't
-- contain a space, Nothing should be returned
--
-- (NB! There are obviously other corner cases like the inputs " " and
-- "a b c", but you don't need to worry about those here)

const_val_q1_True = True
const_val_q1_False = False
const_val_q1_zero = 0
const_val_q1_one = 1
const_val_q1_empty_list = []

split :: String -> Maybe (String, String)
split s = case words s of
  [x, y] -> Just (x, y)
  _      -> Nothing

checkNumber :: (String, String) -> Maybe (String, String)
checkNumber (for, sur)
  | any isDigit for || any isDigit sur = Nothing
  | otherwise = Just (for, sur)

checkCapitals :: (String, String) -> Maybe (String, String)
checkCapitals (for, sur)
  | all isUpper [head for, head sur] = Just (for, sur)
  | otherwise = Nothing

------------------------------------------------------------------------------
-- Ex 2: Given a list of players and their scores (as [(String,Int)]),
-- and two player names, return the name of the player who has more
-- points (wrapped in a Just), or Nothing if either of the players
-- doesn't exist.
--
-- In the case of a draw, prefer the first player.
--
-- Use the function
--   lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- and either do-notation (easier) or ?> chaining (trickier!)
--
-- Examples:
--   winner [("ender",13),("orson",6),("scott",5)] "ender" "orson"
--     ==> Just "ender"
--   winner [("ender",13),("orson",6),("scott",5)] "orson" "ender"
--     ==> Just "ender"
--   winner [("ender",13),("orson",6),("scott",5)] "orson" "scott"
--     ==> Just "orson"
--   winner [("ender",13),("orson",6),("scott",5)] "orson" "ridley"
--     ==> Nothing
--   winner [("a",1),("b",1)] "a" "b"
--     ==> Just "a"

winner :: [(String, Int)] -> String -> String -> Maybe String
winner scores player1 player2 = do
  score1 <- lookup player1 scores
  score2 <- lookup player2 scores
  return $ if score1 < score2 
    then player2 
  else player1

------------------------------------------------------------------------------
-- Ex 3: given a list of indices and a list of values, return the sum
-- of the values in the given indices. You should fail if any of the
-- indices is too large or too small.
--
-- Use the Maybe monad, i.e. the >>= operator or do-notation.
--
-- Hint! implement a function safeIndex :: [a] -> Int -> Maybe a
--
-- Examples:
--  selectSum [0..10] [4,6,9]
--    Just 19
--  selectSum [0..10] [4,6,9,20]
--    Nothing

const_val_q3_True = True
const_val_q3_False = False
const_val_q3_zero = 0
const_val_q3_one = 1
const_val_q3_empty_list = []

getArrayLength :: [a] -> Int
getArrayLength xsListing = length xsListing

getGreaterThan :: Int -> Int -> Bool
getGreaterThan a b = a > b

getGreaterThanEqual :: Int -> Int -> Bool
getGreaterThanEqual a b = a >= b

getLessThan :: Int -> Int -> Bool
getLessThan a b = a < b

getLessThanEqual :: Int -> Int -> Bool
getLessThanEqual a b = a <= b

-- 

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs idx
  | getGreaterThan idx 0 || getLessThan idx (getArrayLength xs) = Just (xs !! idx)
  | otherwise = Nothing

selectSum :: Num a => [a] -> [Int] -> Maybe a
selectSum xs is = if all 
  (\idx -> getGreaterThanEqual idx 0 && getLessThan idx (getArrayLength xs)) is
    then Just $ sum $ mapMaybe (\idx -> safeIndex xs idx) is
  else Nothing

------------------------------------------------------------------------------
-- Ex 4: Here is the Logger monad from the course material. Implement
-- the operation countAndLog which produces the number of elements
-- from the given list that fulfil the given predicate. Additionally,
-- countAndLog should log all elements that fulfil the predicate
-- (using show to turn them into strings).
--
-- Examples:
--   countAndLog even [0,1,2,3,4,5]
--     ==> Logger ["0","2","4"] 3

const_val_q4_True = True
const_val_q4_False = False
const_val_q4_zero = 0
const_val_q4_one = 1
const_val_q4_empty_list = []

data Logger a = Logger [String] a
  deriving (Show, Eq)

msg :: String -> Logger ()
msg s = Logger [s] ()

instance Functor Logger where
  fmap f (Logger l a) = Logger l (f a)

instance Monad Logger where
  return x = Logger [] x
  Logger la a >>= f = Logger (la++lb) b
    where Logger lb b = f a

-- This is an Applicative instance that works for any monad, you
-- can just ignore it for now. We'll get back to Applicative later.
instance Applicative Logger where
  pure = return
  (<*>) = ap

getAddResult :: Int -> Int -> Int
getAddResult x_val y_val = x_val + y_val

countAndLog :: Show a => (a -> Bool) -> [a] -> Logger Int
countAndLog p xs = foldr (countIfAndLog p) (return 0) xs

countIfAndLog :: Show a => (a -> Bool) -> a -> Logger Int -> Logger Int
countIfAndLog p x acc
  | p x = logAndIncrement x acc
  | otherwise = acc

logAndIncrement :: Show a => a -> Logger Int -> Logger Int
logAndIncrement x acc = do
  msg (show x)
  n <- acc
  return (getAddResult n const_val_q4_one)

------------------------------------------------------------------------------
-- Ex 5: You can find the Bank and BankOp code from the course
-- material in the module Examples.Bank (file
-- exercises/Examples/Bank.hs), which has been imported into this
-- namespace.
--
-- Implement a BankOp balance that produces the balance of the given
-- account. Produce 0 if the account does not exist. The balance
-- operation shouldn't change the state of the Bank. The functions
-- from Data.Map are available under the prefix Map.

const_val_q5_True = True
const_val_q5_False = False
const_val_q5_zero = 0
const_val_q5_one = 1
const_val_q5_seven = 7
const_val_q5_ten = 10
const_val_q5_empty_list = []
const_val_q5_str1 = "harry"
const_val_q5_str2 = "cedric"
const_val_q5_str3 = "ginny"

exampleBank :: Bank
exampleBank = (Bank (Map.fromList 
  [(const_val_q5_str1,const_val_q5_ten),
    (const_val_q5_str2,const_val_q5_seven),
    (const_val_q5_str3,const_val_q5_one)]))

balance :: String -> BankOp Int
balance accountName = BankOp getBalance
  where 
    getBalance (Bank accounts) 
      = (Map.findWithDefault const_val_q5_zero accountName accounts, Bank accounts)

------------------------------------------------------------------------------
-- Ex 6: Using the operations balance, withdrawOp and depositOp, and
-- chaining (+>), implement the BankOp rob, which transfers all the
-- money from one account to another account.
--
-- Examples:
--   runBankOp (balance "harry") exampleBank
--     ==> (10,Bank (fromList [("cedric",7),("ginny",1),("harry",10)]))
--   runBankOp (balance "sean") exampleBank
--     ==> (0,Bank (fromList [("cedric",7),("ginny",1),("harry",10)]))
--   runBankOp (rob "cedric" "ginny") exampleBank
--     ==> ((),Bank (fromList [("cedric",0),("ginny",8),("harry",10)]))
--   runBankOp (rob "sean" "ginny") exampleBank
--     ==> ((),Bank (fromList [("cedric",7),("ginny",1),("harry",10)]))

robHelper :: String -> String -> Int -> BankOp ()
robHelper from to amount = withdrawOp from amount +> \_ -> depositOp to amount

rob :: String -> String -> BankOp ()
rob from to = balance from +> robHelper from to

------------------------------------------------------------------------------
-- Ex 7: using the State monad, write the operation `update` that first
-- multiplies the state by 2 and then adds one to it. The state has
-- type Int.
--
-- Example:
--  runState update 3
--    ==> ((),7)

const_val_q7_True = True
const_val_q7_False = False
const_val_q7_zero = 0
const_val_q7_one = 1
const_val_q7_two = 2
const_val_q7_empty_list = []

getMultiplication :: Int -> Int -> Int
getMultiplication a b = a * b 

update :: State Int ()
update = modify (\x -> getAddResult (getMultiplication x const_val_q7_two) const_val_q7_one)

------------------------------------------------------------------------------
-- Ex 8: Checking that parentheses are balanced with the State monad.
--
-- Do this by implementing the function paren, which updates the state
-- based on a single character. A '(' should increase the state, and a
-- ')' should decrease the state. If the state goes to -1 (there are
-- more closing than opening parentheses), it should stay there to
-- indicate that a parenthesis error was encountered.
--
-- After you've implemented paren, the given definition of parensMatch
-- should work.
--
-- Examples:
--   runState (paren '(') 3    ==> ((),4)
--   runState (paren ')') 3    ==> ((),2)
--   runState (paren ')') 0    ==> ((),-1)
--   runState (paren ')') (-1) ==> ((),-1)
--   runState (paren '(') (-1) ==> ((),-1)
--   parensMatch "()"          ==> True
--   parensMatch "("           ==> False
--   parensMatch "())"         ==> False
--   parensMatch "(()(()()))"  ==> True
--   parensMatch "(()((()))"   ==> False
--   parensMatch "(()))("      ==> False

const_val_q8_True = True
const_val_q8_False = False
const_val_q8_zero = 0
const_val_q8_one = 1
const_val_q8_two = 2
const_val_q8_empty_list = []
const_val_q8_str1 = "("

paren :: Char -> State Int ()
paren ch = do s <- get
              if (s == -1)
              then return ()
              else modify (\s -> s + if ch == '(' 
                then const_val_q8_one 
              else -const_val_q8_one)

parensMatch :: String -> Bool
parensMatch s = getEqualThan count const_val_q8_zero
  where (_,count) = runState (mapM_ paren s) const_val_q8_zero

------------------------------------------------------------------------------
-- Ex 9: using a state of type [(a,Int)] we can keep track of the
-- numbers of occurrences of elements of type a. For instance
-- [('a',1),('x',3)] means that we've seen one 'a' and three 'x's.
--
-- Implement a State monad operation count that registers the
-- occurrence of the given value.
--
-- That is, the operation `count x` should fetch the pair `(x,n)` from
-- the state, and replace it with the pair `(x,n+1)`. If no such pair
-- is found, the operation should add `(x,1)` to the state.
--
-- Examples:
--  runState (count True) []
--    ==> ((),[(True,1)])
--  runState (count 7) []
--    ==> ((),[(7,1)])
--  runState (count 'a') [('a',1),('b',3)]
--    ==> ((),[('a',2),('b',3)])
--  runState (count 'a' >> count 'b' >> count 'a') []
--    ==> ((),[('a',2),('b',1)])
--
-- PS. The order of the list of pairs doesn't matter

const_val_q9_True = True
const_val_q9_False = False
const_val_q9_zero = 0
const_val_q9_one = 1
const_val_q9_two = 2
const_val_q9_empty_list = []

updateCounter :: Eq a => a -> State [(a, Int)] ()
updateCounter x = modify (\lst -> updateCountHelper x lst)

updateCountHelper :: Eq a => a -> [(a, Int)] -> [(a, Int)]
updateCountHelper x lst =
  case lookup x lst of
    Just n  -> [(x, getAddResult n const_val_q9_one)] ++ filter (\(y, _) -> y /= x) lst
    Nothing -> [(x, const_val_q9_one)] ++ lst

count :: Eq a => a -> State [(a, Int)] ()
count x = updateCounter x

------------------------------------------------------------------------------
-- Ex 10: Implement the operation occurrences, which
--   1. runs the count operation on each element in the input list
--   2. finally produces the number of different items stored in the
--      state
--
-- In other words, use the state monad to count how many unique values
-- occur in a list.
--
-- Examples:
--  runState (occurrences [True,True,True,False,False]) []
--    ==> (2,[(True,3),(False,2)])
--  runState (occurrences [5,5,6,6,5,6,7]) []
--    ==> (3,[(5,3),(6,3),(7,1)])
--  runState (occurrences [True,False]) [(True,1)]
--    ==> (2,[(True,2),(False,1)])
--  runState (occurrences [4,7]) [(2,1),(3,1)]
--    ==> (4,[(2,1),(3,1),(4,1),(7,1)])

const_val_q10_True = True
const_val_q10_False = False
const_val_q10_zero = 0
const_val_q10_one = 1
const_val_q10_two = 2
const_val_q10_empty_list = []

updateCount :: Eq a => a -> State [(a, Int)] ()
updateCount x = modify (\counts -> incrementCount x counts)

getEqualThan :: Int -> Int -> Bool
getEqualThan a b = a == b

incrementCount :: Eq a => a -> [(a, Int)] -> [(a, Int)]
incrementCount x_val [] = [(x_val, const_val_q10_one)]
incrementCount x_val ((y_val, count) : ysListing)
  | x_val == y_val = (y_val, getAddResult count const_val_q10_one) : ysListing
  | otherwise = (y_val, count) : incrementCount x_val ysListing

occurrences :: Eq a => [a] -> State [(a, Int)] Int
occurrences xsListing = do
  mapM_ updateCount xsListing
  gets length

