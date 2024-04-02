-- Exercise set 4a:
--
--  * using type classes
--  * working with lists
--
-- Type classes you'll need
--  * Eq
--  * Ord
--  * Num
--  * Fractional
--
-- Useful functions:
--  * maximum
--  * minimum
--  * sort

module Set4a where

import Mooc.Todo
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Array

------------------------------------------------------------------------------
-- Ex 1: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--   allEqual [] ==> True
--   allEqual [1,2,3] ==> False
--   allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!

const_val_q1True = True
const_val_q1False = False
allEqual :: Eq a => [a] -> Bool
allEqual [] = const_val_q1True
allEqual (x:xs) = checkForEquality x xs

checkForEquality :: Eq a => a -> [a] -> Bool
checkForEquality _ [] = const_val_q1True
checkForEquality y (x:xs)
    | y == x = checkForEquality y xs
    | otherwise = const_val_q1False

------------------------------------------------------------------------------
-- Ex 2: implement the function distinct which returns True if all
-- values in a list are different.
--
-- Hint: a certain function from the lecture material can make this
-- really easy for you.
--
-- Examples:
--   distinct [] ==> True
--   distinct [1,1,2] ==> False
--   distinct [1,2] ==> True

const_val_q2True = True
const_val_q2False = False
distinct :: Eq a => [a] -> Bool
distinct [] = const_val_q2True
distinct (x_val:xs_listing) = not (elem x_val xs_listing) && distinct xs_listing

------------------------------------------------------------------------------
-- Ex 3: implement the function middle that returns the middle value
-- (not the smallest or the largest) out of its three arguments.
--
-- The function should work on all types in the Ord class. Give it a
-- suitable type signature.
--
-- Examples:
--   middle 'b' 'a' 'c'  ==> 'b'
--   middle 1 7 3        ==> 3

middle :: Ord a => a -> a -> a -> a
middle a b c
    | (b <= a && a <= c) || (c <= a && a <= b) = a
    | (a <= b && b <= c) || (c <= b && b <= a) = b
    | otherwise = c

------------------------------------------------------------------------------
-- Ex 4: return the range of an input list, that is, the difference
-- between the smallest and the largest element.
--
-- Your function should work on all suitable types, like Float and
-- Int. You'll need to add _class constraints_ to the type of range.
--
-- It's fine if your function doesn't work for empty inputs.
--
-- Examples:
--   rangeOf [4,2,1,3]          ==> 3
--   rangeOf [1.5,1.0,1.1,1.2]  ==> 0.5

const_str_q4_empty_list = "Empty list"
const_val_q40 = 0
const_val_q4True  = True
const_val_q4False = False
rangeOf :: (Num a, Ord a) => [a] -> a
rangeOf [] = error const_str_q4_empty_list
rangeOf [x] = 0
rangeOf xs = let sorted = sort xs
                 smallest = head sorted
                 largest = last sorted
             in largest - smallest

------------------------------------------------------------------------------
-- Ex 5: given a (non-empty) list of (non-empty) lists, return the longest
-- list. If there are multiple lists of the same length, return the list that
-- has the smallest _first element_.
--
-- (If multiple lists have the same length and same first element,
-- you can return any one of them.)
--
-- Give the function "longest" a suitable type.
--
-- Challenge: Can you solve this exercise without sorting the list of lists?
--
-- Examples:
--   longest [[1,2,3],[4,5],[6]] ==> [1,2,3]
--   longest ["bcd","def","ab"] ==> "bcd"

const_q4_empty_list = []
longest :: Ord a => [[a]] -> [a]
longest lists = foldr (\list acc -> 
    if length list >= length acc && 
        (length list > length acc 
        || head list <= head acc) 
        then list else acc) const_q4_empty_list lists

------------------------------------------------------------------------------
-- Ex 6: Implement the function incrementKey, that takes a list of
-- (key,value) pairs, and adds 1 to all the values that have the given key.
--
-- You'll need to add _class constraints_ to the type of incrementKey
-- to make the function work!
--
-- The function needs to be generic and handle all compatible types,
-- see the examples.
--
-- Examples:
--   incrementKey True [(True,1),(False,3),(True,4)] ==> [(True,2),(False,3),(True,5)]
--   incrementKey 'a' [('a',3.4)] ==> [('a',4.4)]

const_q6_val_zero = 0
const_q6_val_one = 1
const_q6_empty_list = []
incrementKey :: (Eq k, Num v) => k -> [(k, v)] -> [(k, v)]
incrementKey _ [] = const_q6_empty_list
incrementKey targetKey ((k,v):kvs)
    | k == targetKey = (k, v + 1) : incrementKey targetKey kvs
    | otherwise      = (k, v) : incrementKey targetKey kvs

------------------------------------------------------------------------------
-- Ex 7: compute the average of a list of values of the Fractional
-- class.
--
-- There is no need to handle the empty list case.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional

const_q7_val_zero = 0
const_q7_val_one = 1
average :: Fractional a => [a] -> a
average xs = total / count
  where
    total = foldl (+) 0 xs
    count = fromIntegral $ foldl (\acc _ -> acc + const_q7_val_one) const_q7_val_zero xs

------------------------------------------------------------------------------
-- Ex 8: given a map from player name to score and two players, return
-- the name of the player with more points. If the players are tied,
-- return the name of the first player (that is, the name of the
-- player who comes first in the argument list, player1).
--
-- If a player doesn't exist in the map, you can assume they have 0 points.
--
-- Hint: Map.findWithDefault can make this simpler
--
-- Examples:
--   winner (Map.fromList [("Bob",3470),("Jane",2130),("Lisa",9448)]) "Jane" "Lisa"
--     ==> "Lisa"
--   winner (Map.fromList [("Mike",13607),("Bob",5899),("Lisa",5899)]) "Lisa" "Bob"
--     ==> "Lisa"

const_q8_val_zero = 0
const_q8_val_one = 1

winner :: Map.Map String Int -> String -> String -> String
winner scores p1 p2 = determineWinner scores p1 p2

getPlayerScore :: Map.Map String Int -> String -> Int
getPlayerScore scores player = Map.findWithDefault const_q8_val_zero player scores

determineWinner :: Map.Map String Int -> String -> String -> String
determineWinner scores p1 p2
    | getPlayerScore scores p1 >= getPlayerScore scores p2 = p1
    | otherwise = p2

------------------------------------------------------------------------------
-- Ex 9: compute how many times each value in the list occurs. Return
-- the frequencies as a Map from value to Int.
--
-- Challenge 1: try using Map.alter for this
--
-- Challenge 2: use foldr to process the list
--
-- Example:
--   freqs [False,False,False,True]
--     ==> Map.fromList [(False,3),(True,1)]

const_q9_val_one = 1

freqs :: (Ord a) => [a] -> Map.Map a Int
freqs xs = foldr freqIncrement Map.empty xs

freqIncrement :: Ord a => a -> Map.Map a Int -> Map.Map a Int
freqIncrement key freqMap = Map.insertWith (+) key const_q9_val_one freqMap

------------------------------------------------------------------------------
-- Ex 10: recall the withdraw example from the course material. Write a
-- similar function, transfer, that transfers money from one account
-- to another.
--
-- However, the function should not perform the transfer if
-- * the from account doesn't exist,
-- * the to account doesn't exist,
-- * the sum is negative,
-- * or the from account doesn't have enough money.
--
-- Hint: there are many ways to implement this logic. Map.member or
-- Map.notMember might help.
--
-- Examples:
--   let bank = Map.fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Mike" 20 bank
--     ==> fromList [("Bob",80),("Mike",70)]
--   transfer "Bob" "Mike" 120 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Lisa" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Lisa" "Mike" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]

const_q10_val_zero = 0
const_q10_val_one = 1
transfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer from to amount bank
    | not (checkAccoutExistance from bank) 
        || not (checkAccoutExistance to bank) 
        || not (verifyTransferAccount amount) 
        || not (hasEnoughMoney (Map.findWithDefault const_q10_val_zero from bank) amount) = bank
    | otherwise = performTransfer from to amount bank

checkAccoutExistance :: String -> Map.Map String Int -> Bool
checkAccoutExistance acc bank = Map.member acc bank

verifyTransferAccount :: Int -> Bool
verifyTransferAccount amount = amount > const_q10_val_zero

hasEnoughMoney :: Int -> Int -> Bool
hasEnoughMoney balance amount = balance >= amount

performTransfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
performTransfer from to amount bank =
    let fromBalance = Map.findWithDefault const_q10_val_zero from bank
        toBalance = Map.findWithDefault const_q10_val_zero to bank
        newFromBalance = fromBalance - amount
        newToBalance = toBalance + amount
    in Map.adjust (const newFromBalance) from $ Map.adjust (const newToBalance) to bank

------------------------------------------------------------------------------
-- Ex 11: given an Array and two indices, swap the elements in the indices.
--
-- Example:
--   swap 2 3 (array (1,4) [(1,"one"),(2,"two"),(3,"three"),(4,"four")])
--         ==> array (1,4) [(1,"one"),(2,"three"),(3,"two"),(4,"four")]

swap :: Ix i => i -> i -> Array i a -> Array i a
swap i_idx j_idx arr = arr // [(i_idx, arr ! j_idx), (j_idx, arr ! i_idx)]

swapIndices :: Ord i => i -> i -> (i, i)
swapIndices i_idx j_idx = if i_idx <= j_idx then (i_idx, j_idx) else (j_idx, i_idx)

------------------------------------------------------------------------------
-- Ex 12: given an Array, find the index of the largest element. You
-- can assume the Array isn't empty.
--
-- You may assume that the largest element is unique.
--
-- Hint: check out Data.Array.indices or Data.Array.assocs

maxIndex :: (Ix i, Ord a) => Array i a -> i
maxIndex arr = maxIndexFinder arr (assocs arr) (head $ indices arr)

maxIndexFinder :: (Ix i, Ord a) => Array i a -> [(i, a)] -> i -> i
maxIndexFinder _ [] maxIdx = maxIdx
maxIndexFinder arr ((idx, val):xs) maxIdx
    | val > arr ! maxIdx = maxIndexFinder arr xs idx
    | otherwise = maxIndexFinder arr xs maxIdx
