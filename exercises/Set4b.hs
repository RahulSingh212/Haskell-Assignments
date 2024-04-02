-- Exercise set 4b: folds

module Set4b where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: countNothings with a fold. The function countNothings from
-- the course material can be implemented using foldr. Your task is to
-- define countHelper so that the following definition of countNothings
-- works.
--
-- Hint: You can start by trying to add a type signature for countHelper.
--
-- Challenge: look up the maybe function and use it in countHelper.
--
-- Examples:
--   countNothings []  ==>  0
--   countNothings [Just 1, Nothing, Just 3, Nothing]  ==>  2

const_q1_val_True = True
const_q1_val_False = False
const_q1_val_zero = 0
const_q1_val_one = 1
countNothings :: [Maybe a] -> Int
countNothings = foldr countHelper const_q1_val_zero

countHelper :: Maybe a -> Int -> Int
countHelper mx preVal = preVal + countForNothing mx

countForNothing :: Maybe a -> Int
countForNothing Nothing = const_q1_val_one
countForNothing _       = const_q1_val_zero

------------------------------------------------------------------------------
-- Ex 2: myMaximum with a fold. Just like in the previous exercise,
-- define maxHelper so that the given definition of myMaximum works.
--
-- Examples:
--   myMaximum []  ==>  0
--   myMaximum [1,3,2]  ==>  3

const_q2_val_True = True
const_q2_val_False = False
const_q2_val_zero = 0
const_q2_val_one = 1
myMaximum :: [Int] -> Int
myMaximum [] = const_q2_val_zero
myMaximum (x_val:xs_listing) = foldr maxHelper x_val xs_listing

maxHelper :: Int -> Int -> Int
maxHelper = maxWithFinder

maxWithFinder :: Int -> Int -> Int
maxWithFinder a_val b_val
  | a_val >= b_val = a_val
  | otherwise = b_val

------------------------------------------------------------------------------
-- Ex 3: compute the sum and length of a list with a fold. Define
-- slHelper and slStart so that the given definition of sumAndLength
-- works. This could be used to compute the average of a list.
--
-- Start by giving slStart and slHelper types.
--
-- Examples:
--   sumAndLength []             ==>  (0.0,0)
--   sumAndLength [1.0,2.0,4.0]  ==>  (7.0,3)

const_q3_val_True = True
const_q3_val_False = False
const_q3_val_double_zero = 0.0
const_q3_val_zero = 0
const_q3_val_one = 1
sumAndLength :: [Double] -> (Double, Int)
sumAndLength xs = foldr slHelper slStart xs

slStart :: (Double, Int)
slStart = (const_q3_val_double_zero, const_q3_val_zero)

slHelper :: Double -> (Double, Int) -> (Double, Int)
slHelper x_val (accSum, accLen) = (accSum + x_val, accLen + const_q3_val_one)

------------------------------------------------------------------------------
-- Ex 4: implement concat with a fold. Define concatHelper and
-- concatStart so that the given definition of myConcat joins inner
-- lists of a list.
--
-- Examples:
--   myConcat [[]]                ==> []
--   myConcat [[1,2,3],[4,5],[6]] ==> [1,2,3,4,5,6]

const_q4_val_True = True
const_q4_val_False = False
const_q4_val_double_zero = 0.0
const_q4_val_zero = 0
const_q4_val_one = 1
const_q4_empty_list = []

myConcat :: [[a]] -> [a]
myConcat = foldr concatHelper concatStart

concatStart :: [a]
concatStart = const_q4_empty_list

concatHelper :: [a] -> [a] -> [a]
concatHelper value acc = foldr (:) acc value

------------------------------------------------------------------------------
-- Ex 5: get all occurrences of the largest number in a list with a
-- fold. Implement largestHelper so that the given definition of largest works.
--
-- Examples:
--   largest [] ==> []
--   largest [1,3,2] ==> [3]
--   largest [1,3,2,3] ==> [3,3]

const_q5_val_True = True
const_q5_val_False = False
const_q5_val_double_zero = 0.0
const_q5_val_zero = 0
const_q5_val_one = 1
const_q5_empty_list = []

largestStart :: [Int]
largestStart = const_q5_empty_list

largestHelper :: Int -> [Int] -> [Int]
largestHelper x_val val_rem@(y:_)
  | x_val > y = [x_val]
  | x_val == y = x_val : val_rem
  | otherwise = val_rem
largestHelper x_val [] = [x_val]

largest :: [Int] -> [Int]
largest xs_listing = foldr largestHelper largestStart xs_listing

------------------------------------------------------------------------------
-- Ex 6: get the first element of a list with a fold. Define
-- headHelper so that the given definition of myHead works.
--
-- Start by giving headHelper a type.
--
-- Examples:
--   myHead []  ==>  Nothing
--   myHead [1,2,3]  ==>  Just 1

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead listing = foldr headHelper Nothing listing

headHelper :: a -> Maybe a -> Maybe a
headHelper x_val nVal = Just x_val

------------------------------------------------------------------------------
-- Ex 7: get the last element of a list with a fold. Define lasthelper
-- so that the given definition of myLast works.
--
-- Start by giving lastHelper a type.
--
-- Examples:
--   myLast [] ==> Nothing
--   myLast [1,2,3] ==> Just 3

const_q7_val_True = True
const_q7_val_False = False
const_q7_val_double_zero = 0.0
const_q7_val_zero = 0
const_q7_val_one = 1
const_q7_empty_list = []

lastHelper :: a -> Maybe a -> Maybe a
lastHelper currValue Nothing = Just currValue
lastHelper irrVal acc = acc

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast listingVal = foldr lastHelper Nothing listingVal