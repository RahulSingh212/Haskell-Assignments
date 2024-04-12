module Set11a where

import Control.Monad
import Data.List
import System.IO

import Mooc.Todo

-- Lecture 11:
--   * The IO type
--   * do-notation
--
-- Useful functions / operations:
--   * putStrLn
--   * getLine
--   * readLn
--   * replicateM
--   * readFile
--   * lines
--
-- Do not add any new imports! E.g. Data.IORef is forbidden.

------------------------------------------------------------------------------
-- Ex 1: define an IO operation hello that prints two lines. The
-- first line should be HELLO and the second one WORLD

const_val_q1_True = True
const_val_q1_False = False
const_val_q1_zero = 0
const_val_q1_one = 1
const_val_q1_empty_list = []
const_val_q1_HELLO = "HELLO"
const_val_q1_WORLD = "WORLD"

hello :: IO ()
hello = do
  putStrLn const_val_q1_HELLO
  putStrLn const_val_q1_WORLD

------------------------------------------------------------------------------
-- Ex 2: define the IO operation greet that takes a name as an
-- argument and prints a line "HELLO name".

const_val_q2_True = True
const_val_q2_False = False
const_val_q2_zero = 0
const_val_q2_one = 1
const_val_q2_empty_list = []
const_val_q2_HELLO = "HELLO "

greet :: String -> IO ()
greet name = putStrLn $ const_val_q2_HELLO ++ name

------------------------------------------------------------------------------
-- Ex 3: define the IO operation greet2 that reads a name from the
-- keyboard and then greets that name like the in the previous
-- exercise.
--
-- Try to use the greet operation in your solution.

greet2 :: IO ()
greet2 = do
  entered_name <- getLine
  greet entered_name

------------------------------------------------------------------------------
-- Ex 4: define the IO operation readWords n which reads n lines from
-- the user and produces them as a list, in alphabetical order.
--
-- Example in GHCi:
--   Set11> readWords 3
--   bob
--   alice
--   carl
--   ["alice","bob","carl"]

readLines :: Int -> IO [String]
readLines n = replicateM n getLine

sortLines :: [String] -> [String]
sortLines = sort

readWords :: Int -> IO [String]
readWords n = do
  lines <- readLines n
  return (sortLines lines)

------------------------------------------------------------------------------
-- Ex 5: define the IO operation readUntil f, which reads lines from
-- the user and returns them as a list. Reading is stopped when f
-- returns True for a line. (The value for which f returns True is not
-- returned.)
--
-- Example in GHCi:
--   *Set11> readUntil (=="STOP")
--   bananas
--   garlic
--   pakchoi
--   STOP
--   ["bananas","garlic","pakchoi"]

const_val_q5_True = True
const_val_q5_False = False
const_val_q5_zero = 0
const_val_q5_one = 1
const_val_q5_empty_list = []
const_val_q5_HELLO = "HELLO "
const_val_q5_WORLD = "WORLD"

readUntil :: (String -> Bool) -> IO [String]
readUntil f = go const_val_q5_empty_list
  where
    go acc = do
      line <- getLine
      if f line
        then return (reverse acc)
        else go (line : acc)

------------------------------------------------------------------------------
-- Ex 6: given n, print the numbers from n to 0, one per line

const_val_q6_True = True
const_val_q6_False = False
const_val_q6_zero = 0
const_val_q6_one = 1
const_val_q6_empty_list = []

countdownPrint :: Int -> IO ()
countdownPrint n = mapM_ putStrLn (map show [n, n-const_val_q1_one .. const_val_q1_zero])

------------------------------------------------------------------------------
-- Ex 7: isums n should read n numbers from the user (one per line) and
--   1) after each number, print the running sum up to that number
--   2) finally, produce the sum of all numbers
--
-- Example:
--   1. run `isums 3`
--   2. user enters '3', should print '3'
--   3. user enters '5', should print '8' (3+5)
--   4. user enters '1', should print '9' (3+5+1)
--   5. produces 9

const_val_q7_True = True
const_val_q7_False = False
const_val_q7_zero = 0
const_val_q7_one = 1
const_val_q7_empty_list = []

getAddResults :: Int -> Int -> Int
getAddResults x y = x + y

isums :: Int -> IO Int
isums n = go const_val_q7_zero n
  where
    go acc 0 = return acc
    go acc idx_val = do
      num <- readLn
      let newSum = getAddResults acc num
      print newSum
      go newSum (idx_val - const_val_q7_one)

------------------------------------------------------------------------------
-- Ex 8: when is a useful function, but its first argument has type
-- Bool. Write a function that behaves similarly but the first
-- argument has type IO Bool.

const_val_q8_True = True
const_val_q8_False = False
const_val_q8_zero = 0
const_val_q8_one = 1
const_val_q8_empty_list = []

executeWhen :: IO Bool -> IO () -> IO ()
executeWhen cond op = do
  result <- cond
  when result op

whenM :: IO Bool -> IO () -> IO ()
whenM = executeWhen

------------------------------------------------------------------------------
-- Ex 9: implement the while loop. while condition operation should
-- run operation as long as condition returns True.
--
-- Examples:
--   -- prints nothing
--   while (return False) (putStrLn "IMPOSSIBLE")
--
--   -- prints YAY! as long as the user keeps answering Y
--   while ask (putStrLn "YAY!")

-- used in an example

const_val_q9_True = True
const_val_q9_False = False
const_val_q9_zero = 0
const_val_q9_one = 1
const_val_q9_empty_list = []

executeWhile :: IO Bool -> IO () -> IO ()
executeWhile cond op = do
  result <- cond
  when result $ do
    op
    executeWhile cond op

while :: IO Bool -> IO () -> IO ()
while = executeWhile


------------------------------------------------------------------------------
-- Ex 10: given a string and an IO operation, print the string, run
-- the IO operation, print the string again, and finally return what
-- the operation returned.
--
-- Note! the operation should be run only once
--
-- Examples:
--   debug "CIAO" (return 3)
--     - prints two lines that contain CIAO
--     - returns the value 3
--   debug "BOOM" getLine
--     1. prints "BOOM"
--     2. reads a line from the user
--     3. prints "BOOM"
--     4. returns the line read from the user

const_val_q10_True = True
const_val_q10_False = False
const_val_q10_zero = 0
const_val_q10_one = 1
const_val_q10_empty_list = []

debugIO :: String -> IO a -> IO a
debugIO s op = do
  putStrLn s
  result <- op
  putStrLn s
  return result

debug :: String -> IO a -> IO a
debug = debugIO
