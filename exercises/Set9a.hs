-- Welcome to the first exercise set of part 2 of the Haskell Mooc!
-- Edit this file according to the instructions, and check your
-- answers with
--
--   stack runhaskell Set9aTest.hs
--
-- You can also play around with your answers in GHCi with
--
--   stack ghci Set9a.hs

module Set9a where

import Data.Char
import Data.List
import Data.Ord
import Data.Maybe (catMaybes)
import Data.Maybe (mapMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Implement a function workload that takes in the number of
-- exercises a student has to finish, and another number that counts
-- the number of hours each exercise takes.
--
-- If the total number of hours needed for all exercises is over 100,
-- return "Holy moly!" if it is under 10, return "Piece of cake!".
-- Otherwise return "Ok."

const_val_q1_zero = 0
const_val_q1_one = 1
const_val_q1_str1 = "Holy moly!"
const_val_q1_str2 = "Piece of cake!"
const_val_q1_str3 = "Ok."
const_val_q1_str4 = "ELSE"

workload :: Int -> Int -> String
workload nExercises hoursPerExercise
  | totalHours < 10 = const_val_q1_str2
  | totalHours > 100 = const_val_q1_str1
  | totalHours >= 10 = const_val_q1_str3
  | totalHours <= 100 = const_val_q1_str3
  | otherwise = const_val_q1_str3
  where 
    totalHours = nExercises * hoursPerExercise

------------------------------------------------------------------------------
-- Ex 2: Implement the function echo that builds a string like this:
--
--   echo "hello!" ==> "hello!, ello!, llo!, lo!, o!, !, "
--   echo "ECHO" ==> "ECHO, CHO, HO, O, "
--   echo "X" ==> "X, "
--   echo "" ==> ""
--
-- Hint: use recursion

const_val_q2_zero = 0
const_val_q2_one = 1
const_val_q2_empty_string = ""
const_val_q2_str1 = ", "

echo :: String -> String
echo [] = const_val_q2_empty_string
echo (x_val:xsListing) = (x_val : xsListing) ++ const_val_q2_str1 ++ echo xsListing

------------------------------------------------------------------------------
-- Ex 3: A country issues some banknotes. The banknotes have a serial
-- number that can be used to check if the banknote is valid. For a
-- banknote to be valid, either
--  * the third and fifth digits need to be the same
--  * or the fourth and sixth digits need to be the same
--
-- Given a list of bank note serial numbers (strings), count how many
-- are valid.

const_val_q3_zero = 0
const_val_q3_one = 1
const_val_q3_two = 2
const_val_q3_three = 3
const_val_q3_four = 4
const_val_q3_five = 5
const_val_q3_six = 6

isValidSerial :: String -> Bool
isValidSerial serial = length serial >= const_val_q3_six && checkDigits serial

checkDigits :: String -> Bool
checkDigits serial = (serial !! const_val_q3_two == serial !! const_val_q3_four) || (serial !! const_val_q3_three == serial !! const_val_q3_five)

countValid :: [String] -> Int
countValid serials = length (filter isValidSerial serials)

------------------------------------------------------------------------------
-- Ex 4: Find the first element that repeats two or more times _in a
-- row_ in the input list. Return a Nothing value if no element repeats.
--
-- Examples:
--   repeated [1,2,3] ==> Nothing
--   repeated [1,2,2,3,3] ==> Just 2
--   repeated [1,2,1,2,3,3] ==> Just 3

const_val_q4_True = True
const_val_q4_False = False
const_val_q4_Nothing = Nothing

hasRepeats :: Eq a => [a] -> Bool
hasRepeats [] = const_val_q4_False
hasRepeats [_] = const_val_q4_False
hasRepeats (x:y:rest)
  | x == y = True
  | otherwise = hasRepeats (y:rest)

repeated :: Eq a => [a] -> Maybe a
repeated [] = const_val_q4_Nothing
repeated xs
  | hasRepeats xs = Just $ findRepeated xs
  | otherwise = const_val_q4_Nothing

findRepeated :: Eq a => [a] -> a
findRepeated (x:y:rest)
  | x == y = x
  | otherwise = findRepeated (y:rest)

------------------------------------------------------------------------------
-- Ex 5: A laboratory has been collecting measurements. Some of the
-- measurements have failed, so the lab is using the type
--   Either String Int
-- to track the measurements. A Left value represents a failed measurement,
-- while a Right value represents a successful one.
--
-- Compute the sum of all successful measurements. If there are
-- successful measurements, return the sum wrapped in a Right, but if
-- there are none, return Left "no data".
--
-- Examples:
--   sumSuccess [Right 1, Left "it was a snake!", Right 3]
--     ==> Right 4
--   sumSuccess [Left "lab blew up", Left "I was sick"]
--     ==> Left "no data"
--   sumSuccess []
--     ==> Left "no data"

const_val_q5_True = True
const_val_q5_False = False
const_val_q5_str1 = "no data"

hasSuccess :: [Either String Int] -> Bool
hasSuccess = any isRight
  where
    isRight (Right extra1) = const_val_q5_True
    isRight extra2 = const_val_q5_False

sumSuccess :: [Either String Int] -> Either String Int
sumSuccess measurements
  | hasSuccess measurements = Right $ sumSuccessValues measurements
  | otherwise = Left const_val_q5_str1

sumSuccessValues :: [Either String Int] -> Int
sumSuccessValues = sum . catMaybes . map extractSuccess
  where
    extractSuccess (Right x_val) = Just x_val
    extractSuccess extra = Nothing

------------------------------------------------------------------------------
-- Ex 6: A combination lock can either be open or closed. The lock
-- also remembers a code. A closed lock can only be opened with the
-- right code. The code of an open lock can be changed.
--
-- Implement a datatype Lock and the functions isOpen, open, lock,
-- changeCode and the constant aLock as instructed below.
--
-- Examples:
--   isOpen aLock ==> False
--   isOpen (lock aLock) ==> False
--   isOpen (open "1234" aLock) ==> True
--   isOpen (lock (open "1234" aLock)) ==> False
--   isOpen (open "1235" aLock) ==> False
--   isOpen (lock (open "1235" aLock)) ==> False
--   isOpen (open "1234" (changeCode "0000" aLock)) ==> True
--   isOpen (open "0000" (changeCode "0000" aLock)) ==> False
--   isOpen (open "0000" (lock (changeCode "0000" (open "1234" aLock)))) ==> True
--   isOpen (open "1234" (lock (changeCode "0000" (open "1234" aLock)))) ==> False

const_val_q6_True = True
const_val_q6_False = False

data Lock = ClosedLock String | OpenLock String
  deriving Show

aLock :: Lock
aLock = ClosedLock "1234"

isOpen :: Lock -> Bool
isOpen (OpenLock extra) = const_val_q6_True
isOpen extra = const_val_q6_False

open :: String -> Lock -> Lock
open code (ClosedLock c) | code == c = OpenLock c
open extra lock = lock

lock :: Lock -> Lock
lock (OpenLock code) = ClosedLock code
lock l = l

changeCode :: String -> Lock -> Lock
changeCode setNewCode (OpenLock extra) = OpenLock setNewCode
changeCode extra lock = lock

------------------------------------------------------------------------------
-- Ex 7: Here's a type Text that just wraps a String. Implement an Eq
-- instance for Text that ignores all white space (space characters
-- and line returns).
--
-- Hint: Data.Char.isSpace
--
-- Examples
--   Text "abc"  == Text "abc"      ==> True
--   Text "a bc" == Text "ab  c\n"  ==> True
--   Text "abc"  == Text "abcd"     ==> False
--   Text "a bc" == Text "ab  d\n"  ==> False

data Text = Text String
  deriving Show

excludeWhiteSpace :: String -> String
excludeWhiteSpace = filter (not . isSpace)

textEqual :: Text -> Text -> Bool
textEqual (Text a_val) (Text b_val) = excludeWhiteSpace a_val == excludeWhiteSpace b_val

instance Eq Text where
  (==) = textEqual

------------------------------------------------------------------------------
-- Ex 8: We can represent functions or mappings as lists of pairs.
-- For example the list [("bob",13),("mary",8)] means that "bob" maps
-- to 13 and "mary" maps to 8.
--
-- Implement _composition_ for mappings like this. You compose two
-- mappings by looking up each result of the first mapping in the
-- second mapping.
--
-- You may assume there are no repeated first elements of tuples in
-- the argument lists, that is.
--
-- The ordering of the output doesn't matter.
--
-- Hint: remember the function `lookup` from Prelude?
--
-- Note! The order of arguments to `compose` is the other way around
-- compared to e.g. (.): `compose f g` should apply `f` first, then
-- `g`, but `f.g` applies `g` first, then `f`.
--
-- Examples:
--   composing two mappings of size 1:
--     compose [("a",1)] [(1,True)]
--       ==> [("a",True)]
--   nonmatching mappings get ignored:
--     compose [("a",1),("b",2)] [(3,False),(4,True)]
--       ==> []
--   a more complex example: note how "omicron" and "c" are ignored
--     compose [("a","alpha"),("b","beta"),("c","gamma")] [("alpha",1),("beta",2),("omicron",15)]
--       ==> [("a",1),("b",2)]

const_val_q8_Nothing = Nothing

findValue :: Eq a => a -> [(a, b)] -> Maybe b
findValue extra1 [] = const_val_q8_Nothing
findValue key ((k_val, value):rest)
  | key == k_val = Just value
  | otherwise = findValue key rest

compose :: (Eq a, Eq b, Eq c) => [(a, b)] -> [(b, c)] -> [(a, c)]
compose f g = mapMaybe composePair f
  where
    composePair (a, b) = case findValue b g of
      Just c -> Just (a, c)
      Nothing -> const_val_q8_Nothing

------------------------------------------------------------------------------
-- Ex 9: Reorder a list using a list of indices.
--
-- You are given a list of indices (numbers from 0 to n) and an input
-- list (of length n). Each index in the index list tells you where to
-- place the corresponding element from the input list in the output
-- list.
--
-- For example, if the 3rd element of the index list is 7, and the 3rd
-- element of the input list is 'a', the output list should have 'a'
-- at index 7.
--
-- (The index lists discussed in this exercise correspond to permutations in
-- math. In fact, permutations can be multiplied which is a special case of
-- the compose function in the previous exercise. For more information on
-- permutations, see https://en.wikipedia.org/wiki/Permutation)
--
-- Examples:
--   permute [0,1] [True, False] ==> [True, False]
--   permute [1,0] [True, False] ==> [False, True]
--   permute [0,1,2,3] "hask" ==> "hask"
--   permute [2,0,1,3] "hask" ==> "ashk"
--   permute [1,2,3,0] "hask" ==> "khas"
--   permute [2, 1, 0] (permute [2, 1, 0] "foo") ==> "foo"
--   permute [1, 0, 2] (permute [0, 2, 1] [9,3,5]) ==> [5,9,3]
--   permute [0, 2, 1] (permute [1, 0, 2] [9,3,5]) ==> [3,5,9]
--   permute ([1, 0, 2] `multiply` [0, 2, 1]) [9,3,5] ==> [5,9,3]
--   permute ([0, 2, 1] `multiply` [1, 0, 2]) [9,3,5] ==> [3,5,9]

const_val_q9_True = True
const_val_q9_False = False
const_val_q9_zero = 0
const_val_q9_one = 1

type Permutation = [Int]

identity :: Int -> Permutation
identity n = [const_val_q9_zero .. n - const_val_q9_one]

composePermutations :: Permutation -> Permutation -> Permutation
composePermutations p q = map (\i -> p !! (q !! i)) (identity (length p))

permute :: Permutation -> [a] -> [a]
permute indices values = map snd . sortBy (comparing fst) $ zip indices values

permutationCompute :: [Permutation] -> [a] -> [a]
permutationCompute [] values = values
permutationCompute (p:ps) values = permutationCompute ps (permute p values)

multiply :: Permutation -> Permutation -> Permutation
multiply p_val q_val = map (\idx -> p_val !! (q_val !! idx)) (identity (length p_val))