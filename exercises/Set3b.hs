-- Exercise set 3b
--
-- This is a special exercise set. The exercises are about
-- implementing list functions using recursion and pattern matching,
-- without using any standard library functions. For this reason,
-- you'll be working in a limited environment where almost none of the
-- standard library is available.
--
-- At least the following standard library functions are missing:
--  * (++)
--  * head
--  * tail
--  * map
--  * filter
--  * concat
--  * (!!)
--
-- The (:) operator is available, as is list literal syntax [a,b,c].
--
-- Feel free to use if-then-else, guards, and ordering functions (< and > etc.).
--
-- The tests will check that you haven't added imports :)

{-# LANGUAGE NoImplicitPrelude #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: given numbers start, count and end, build a list that starts
-- with count copies of start and ends with end.
--
-- Use recursion and the : operator to build the list.
--
-- Examples:
--   buildList 1 5 2 ==> [1,1,1,1,1,2]
--   buildList 7 0 3 ==> [3]

const_val_q10 = 0
const_val_q11 = 1
buildList :: Int -> Int -> Int -> [Int]
buildList start count end
    | count <= const_val_q10 = [end]
    | otherwise  = start : buildList start (count - const_val_q11) end

------------------------------------------------------------------------------
-- Ex 2: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
--
-- Use recursion and the : operator to build the list.
--
-- Ps. you'll probably need a recursive helper function

const_val_q20 = 0
const_val_q21 = 1
sums :: Int -> [Int]
sums i = go 1 i
  where
    go _ 0 = []
    go start count = sumToNvalues start : go (start + 1) (count - 1)

sumToNvalues :: Int -> Int
sumToNvalues 0 = const_val_q20
sumToNvalues n = n + sumToNvalues (n - const_val_q21)

------------------------------------------------------------------------------
-- Ex 3: define a function mylast that returns the last value of the
-- given list. For an empty list, a provided default value is
-- returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   mylast 0 [] ==> 0
--   mylast 0 [1,2,3] ==> 3

mylast :: a -> [a] -> a
mylast def_val xs = case xs of
    []     -> def_val
    [x_val]    -> x_val
    (_:rem) -> mylast def_val rem

------------------------------------------------------------------------------
-- Ex 4: safe list indexing. Define a function indexDefault so that
--   indexDefault xs i def
-- gets the element at index i in the list xs. If i is not a valid
-- index, def is returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   indexDefault [True] 1 False         ==>  False
--   indexDefault [10,20,30] 0 7         ==>  10
--   indexDefault [10,20,30] 2 7         ==>  30
--   indexDefault [10,20,30] 3 7         ==>  7
--   indexDefault ["a","b","c"] (-1) "d" ==> "d"

const_val_q40 = 0
const_val_q41 = 1
indexDefault :: [a] -> Int -> a -> a
indexDefault [] extra def = def
indexDefault (x_val:xs) idx_val def
    | idx_val < const_val_q40  = def
    | idx_val == const_val_q40 = x_val
    | otherwise                = indexDefault xs (idx_val - const_val_q41) def

------------------------------------------------------------------------------
-- Ex 5: define a function that checks if the given list is in
-- increasing order.
--
-- Use pattern matching and recursion to iterate through the list.
--
-- Examples:
--   sorted [1,2,3] ==> True
--   sorted []      ==> True
--   sorted [2,7,7] ==> True
--   sorted [1,3,2] ==> False
--   sorted [7,2,7] ==> False

const_val_q5True = True
const_val_q5False = False
sorted :: [Int] -> Bool
sorted []       = const_val_q5True
sorted [_]      = const_val_q5True
sorted (x_val:y_val:xs)
    | x_val <= y_val    = sorted (y_val:xs)
    | otherwise = False

------------------------------------------------------------------------------
-- Ex 6: compute the partial sums of the given list like this:
--
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf []       ==>  []
--
-- Use pattern matching and recursion (and the list constructors : and [])

const_val_empty_list = []
sumsOf :: [Int] -> [Int]
sumsOf [] = const_val_empty_list
sumsOf [x_val] = [x_val]
sumsOf (x_val:rem_part) = x_val : partialSumAddition x_val rem_part

partialSumAddition :: Int -> [Int] -> [Int]
partialSumAddition _ [] = const_val_empty_list
partialSumAddition acc (x_val:rem_part) = (acc + x_val) : partialSumAddition (acc + x_val) rem_part

------------------------------------------------------------------------------
-- Ex 7: implement the function merge that merges two sorted lists of
-- Ints into a sorted list
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   merge [1,3,5] [2,4,6] ==> [1,2,3,4,5,6]
--   merge [1,1,6] [1,2]   ==> [1,1,1,2,6]

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x_val:xs) (y_val:ys)
    | x_val <= y_val    = x_val : joinTailMerge xs (y_val:ys)
    | otherwise = y_val : joinTailMerge (x_val:xs) ys

joinTailMerge :: [Int] -> [Int] -> [Int]
joinTailMerge [] ys = ys
joinTailMerge xs [] = xs
joinTailMerge (x_val:xs) (y_val:ys)
    | x_val <= y_val    = x_val : joinTailMerge xs (y_val:ys)
    | otherwise = y_val : joinTailMerge (x_val:xs) ys

------------------------------------------------------------------------------
-- Ex 8: compute the biggest element, using a comparison function
-- passed as an argument.
--
-- That is, implement the function mymaximum that takes
--
-- * a function `bigger` :: a -> a -> Bool
-- * a value `initial` of type a
-- * a list `xs` of values of type a
--
-- and returns the biggest value it sees, considering both `initial`
-- and all element in `xs`.
--
-- Examples:
--   mymaximum (>) 3 [] ==> 3
--   mymaximum (>) 0 [1,3,2] ==> 3
--   mymaximum (>) 4 [1,3,2] ==> 4    -- initial value was biggest
--   mymaximum (<) 4 [1,3,2] ==> 1    -- note changed biggerThan
--   mymaximum (\(a,b) (c,d) -> b > d) ("",0) [("Banana",7),("Mouse",8)]
--     ==> ("Mouse",8)

mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum section initial [] = initial
mymaximum bigger initial (x_val:xs)
    | bigger x_val initial = mymaximum bigger x_val xs
    | otherwise = mymaximum bigger initial xs

secondSectionComparison :: Ord b => (a, b) -> (a, b) -> Bool
secondSectionComparison (_, b1) (_, b2) = b1 > b2

firstSectionComparison :: Ord a => (a, b) -> (a, b) -> Bool
firstSectionComparison (a1, _) (a2, _) = a1 > a2


------------------------------------------------------------------------------
-- Ex 9: define a version of map that takes a two-argument function
-- and two lists. Example:
--
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- If the lists have differing lengths, ignore the trailing elements
-- of the longer list.
--
-- Use recursion and pattern matching. Do not use any library functions.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f [] _ = const_val_empty_list
map2 f _ [] = const_val_empty_list
map2 f (x_val:xs_rem) (y_val:ys_rem) = f x_val y_val : map2 f xs_rem ys_rem

------------------------------------------------------------------------------
-- Ex 10: implement the function maybeMap, which works a bit like a
-- combined map & filter.
---
-- maybeMap is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, no value gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in maybeMap f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- maybeMap Just [1,2,3]
--   ==> [1,2,3]
--
-- maybeMap (\x -> Nothing) [1,2,3]
--   ==> []

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap _ [] = const_val_empty_list
maybeMap f (x_val:xs_listing) =
    let result = case f x_val of
                    Just y_val  -> [y_val]
                    Nothing -> const_val_empty_list
    in result `append` maybeMap f xs_listing

append :: [a] -> [a] -> [a]
append [] ys_listing = ys_listing
append (x_val:xs_listing) ys_listing = x_val : append xs_listing ys_listing
