module Set10a where

import Data.Char
import Data.List
import Data.Char (toLower)
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Given a list, produce a new list where each element of the
-- original list repeats twice.
--
-- Make sure your function works with infinite lists.
--
-- Examples:
--   doublify [7,1,6]          ==>  [7,7,1,1,6,6]
--   take 10 (doublify [0..])  ==>  [0,0,1,1,2,2,3,3,4,4]

const_val_q1_True = True
const_val_q1_False = False
const_val_q1_zero = 0
const_val_q1_one = 1
const_val_q1_empty_list = []

doublify :: [a] -> [a]
doublify = foldr (\x acc -> x : x : acc) const_val_q1_empty_list

------------------------------------------------------------------------------
-- Ex 2: Implement the function interleave that takes two lists and
-- produces a new list that takes elements alternatingly from both
-- lists like this:
--
--   interleave [1,2,3] [4,5,6] ==> [1,4,2,5,3,6]
--
-- If one list runs out of elements before the other, just keep adding
-- elements from the other list.
--
-- Make sure your function also works with infinite lists.
--
-- Examples:
--   interleave [1,2,3] [4,5,6]            ==> [1,4,2,5,3,6]
--   interleave [1,2] [4,5,6,7]            ==> [1,4,2,5,6,7]
--   take 10 (interleave [7,7,7] [1..])    ==> [7,1,7,2,7,3,4,5,6,7]
--   take 10 (interleave [1..] (repeat 0)) ==> [1,0,2,0,3,0,4,0,5,0]

interleave :: [a] -> [a] -> [a]
interleave xsListing ysListing = interleave' xsListing ysListing
  where
    interleave' [] ysListing' = ysListing'
    interleave' xsListing' [] = xsListing'
    interleave' (x:xsListing') (y:ysListing') = x : y : interleave' xsListing' ysListing'

------------------------------------------------------------------------------
-- Ex 3: Deal out cards. Given a list of players (strings), and a list
-- of cards (strings), deal out the cards to the players in a cycle.
--
-- Make sure your function works with infinite inputs as well!
--
-- Examples:
--   deal ["Hercule","Ariadne"] ["Ace","Joker","Heart"]
--     ==> [("Ace","Hercule"),("Joker","Ariadne"),("Heart","Hercule")]
--   take 4 (deal ["a","b","c"] (map show [0..]))
--     ==> [("0","a"),("1","b"),("2","c"),("3","a")]
--   deal ("you":(repeat "me")) ["1","2","3","4"]
--     ==> [("1","you"),("2","me"),("3","me"),("4","me")]
--
-- Hint: remember the functions cycle and zip?

const_val_q3_True = True
const_val_q3_False = False
const_val_q3_zero = 0
const_val_q3_one = 1
const_val_q3_empty_list = []

deal :: [String] -> [String] -> [(String, String)]
deal players cards = cardZipper cards (cycle players)
  where
    cardZipper :: [String] -> [String] -> [(String, String)]
    cardZipper [] extra = const_val_q3_empty_list
    cardZipper extra [] = const_val_q3_empty_list
    cardZipper (c:cs) (p:ps) = (c, p) : cardZipper cs ps

------------------------------------------------------------------------------
-- Ex 4: Compute a running average. Go through a list of Doubles and
-- output a list of averages: the average of the first number, the
-- average of the first two numbers, the first three numbers, and so
-- on.
--
-- Make sure your function works with infinite inputs as well!
--
-- Examples:
--   averages [] ==> []
--   averages [3,2,1] ==> [3.0,2.5,2.0]
--   take 10 (averages [1..]) ==> [1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5]

const_val_q4_True = True
const_val_q4_False = False
const_val_q4_zero = 0
const_val_q4_one = 1
const_val_q4_empty_list = []

averages :: [Double] -> [Double]
averages xs = computeAverages xs 0 0
  where
    computeAverages :: [Double] -> Double -> Double -> [Double]
    computeAverages [] extra1 extra2 = []
    computeAverages (x:xs) sum count =
      let newSum = sum + x
          newCount = count + const_val_q4_one
          avg = newSum / newCount
      in avg : computeAverages xs newSum newCount

------------------------------------------------------------------------------
-- Ex 5: Given two lists, xs and ys, and an element z, generate an
-- infinite list that consists of
--
--  * the elements of xs
--  * z
--  * the elements of ys
--  * z
--  * the elements of xs
--  * ... and so on
--
-- Examples:
--   take 20 (alternate "abc" "def" ',') ==> "abc,def,abc,def,abc,"
--   take 10 (alternate [1,2] [3,4,5] 0) ==> [1,2,0,3,4,5,0,1,2,0]

alternate :: [a] -> [a] -> a -> [a]
alternate xs ys z = listGeneration xs ys z
  where
    listGeneration :: [a] -> [a] -> a -> [a]
    listGeneration xs' ys' z' = concat $ cycle [xs', [z'], ys', [z']]

------------------------------------------------------------------------------
-- Ex 6: Check if the length of a list is at least n. Make sure your
-- function works for infinite inputs.
--
-- Examples:
--   lengthAtLeast 2 [1,2,3] ==> True
--   lengthAtLeast 7 [1,2,3] ==> False
--   lengthAtLeast 10 [0..]  ==> True

const_val_q6_True = True
const_val_q6_False = False
const_val_q6_zero = 0
const_val_q6_one = 1
const_val_q6_empty_list = []

lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast n_val xs = checkLength n_val xs

checkLength :: Int -> [a] -> Bool
checkLength 0 extra = const_val_q6_True
checkLength mth_value [] = const_val_q6_False
checkLength mth_value (extra:ysListing) = checkLength (mth_value-const_val_q6_one) ysListing

------------------------------------------------------------------------------
-- Ex 7: The function chunks should take in a list, and a number n,
-- and return all sublists of length n of the original list. The
-- sublists should be in the order that they appear in the original
-- list. A sublist means a slice, that is, a list of elements
-- a,b,c,... that occur in the original list next to each other and in
-- the same order.
--
-- Make sure your function works with infinite inputs. The function
-- lengthAtLeast can help with this.
--
-- Examples:
--   chunks 2 [1,2,3,4] ==> [[1,2],[2,3],[3,4]]
--   take 4 (chunks 3 [0..]) ==> [[0,1,2],[1,2,3],[2,3,4],[3,4,5]]

const_val_q7_True = True
const_val_q7_False = False
const_val_q7_zero = 0
const_val_q7_one = 1
const_val_q7_empty_list = []

chunks :: Int -> [a] -> [[a]]
chunks n_val xsListing = segregateSections n_val xsListing
  where
    segregateSections :: Int -> [a] -> [[a]]
    segregateSections extra [] = const_val_q7_empty_list
    segregateSections m ysListing@(z:zs)
      | lengthAtLeast n_val ysListing = take n_val ysListing : segregateSections m zs
      | otherwise = const_val_q7_empty_list

------------------------------------------------------------------------------
-- Ex 8: Define a newtype called IgnoreCase, that wraps a value of
-- type String. Define an `Eq` instance for IgnoreCase so that it
-- compares strings in a case-insensitive way.
--
-- To help the tests, also implement the function
--   ignorecase :: String -> IgnoreCase
--
-- Hint: remember Data.Char.toLower
--
-- Examples:
--   ignorecase "abC" == ignorecase "ABc"  ==>  True
--   ignorecase "acC" == ignorecase "ABc"  ==>  False

newtype IgnoreCase = IgnoreCase String

instance Eq IgnoreCase where
  (IgnoreCase str1) == (IgnoreCase str2) = caseIgnoreCompare str1 str2

caseIgnoreCompare :: String -> String -> Bool
caseIgnoreCompare str1 str2 = map toLower str1 == map toLower str2

ignorecase :: String -> IgnoreCase
ignorecase = IgnoreCase

------------------------------------------------------------------------------
-- Ex 9: Here's the Room type and some helper functions from the
-- course material. Define a cyclic Room structure like this:
--
--  * maze1 has the description "Maze"
--    * The direction "Left" goes to maze2
--    * "Right" goes to maze3
--  * maze2 has the description "Deeper in the maze"
--    * "Left" goes to maze3
--    * "Right" goes to maze1
--  * maze3 has the description "Elsewhere in the maze"
--    * "Left" goes to maze1
--    * "Right" goes to maze2
--
-- The variable maze should point to the room maze1.
--
-- Examples:
--   play maze ["Left","Left","Left"]
--      ==> ["Maze","Deeper in the maze","Elsewhere in the maze","Maze"]
--   play maze ["Right","Right","Right","Right"]
--      ==> ["Maze","Elsewhere in the maze","Deeper in the maze","Maze","Elsewhere in the maze"]
--   play maze ["Left","Left","Right"]
--      ==> ["Maze","Deeper in the maze","Elsewhere in the maze","Deeper in the maze"]

const_val_q9_True = True
const_val_q9_False = False
const_val_q9_zero = 0
const_val_q9_one = 1
const_val_q9_empty_list = []
const_val_q9_Maze = "Maze"
const_val_q9_Left = "Left"
const_val_q9_Right = "Right"
const_val_q9_str1 = "Deeper in the maze"
const_val_q9_str2 = "Elsewhere in the maze"

data Room = Room String [(String,Room)]

maze1 :: Room
maze1 = Room const_val_q9_Maze [(const_val_q9_Left, maze2), (const_val_q9_Right, maze3)]

maze2 :: Room
maze2 = Room const_val_q9_str1 [(const_val_q9_Left, maze3), (const_val_q9_Right, maze1)]

maze3 :: Room
maze3 = Room const_val_q9_str2 [(const_val_q9_Left, maze1), (const_val_q9_Right, maze2)]

maze :: Room
maze = maze1

