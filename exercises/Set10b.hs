-- This exercise set hides most of Prelude. You only have access to
-- the Bool, Int and list types, and pattern matching.
--
-- In particular, seq is not available, so you must use pattern
-- matching to force evaluation!

{-# LANGUAGE NoImplicitPrelude #-}

module Set10b where

import Mooc.VeryLimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Define the operator ||| that works like ||, but forces its
-- _right_ argument instead of the left one.
--
-- Examples:
--   False ||| False     ==> False
--   True ||| False      ==> True
--   undefined ||| True  ==> True
--   False ||| undefined ==> an error!

const_val_q1_True = True
const_val_q1_False = False
const_val_q1_zero = 0
const_val_q1_one = 1
const_val_q1_empty_list = []

(|||) :: Bool -> Bool -> Bool
extra1 ||| True = const_val_q1_True
False ||| extra2 = const_val_q1_False
extra3 ||| False = const_val_q1_True

------------------------------------------------------------------------------
-- Ex 2: Define the function boolLength, that returns the length of a
-- list of booleans and forces all of the elements
--
-- Examples:
--   boolLength [False,True,False] ==> 3
--   boolLength [False,undefined]  ==> an error!
--
-- Note that with the ordinary length function,
--   length [False,undefined] ==> 2

const_val_q2_True = True
const_val_q2_False = False
const_val_q2_zero = 0
const_val_q2_one = 1
const_val_q2_empty_list = []

boolLength :: [Bool] -> Int
boolLength [] = const_val_q2_zero
boolLength (x:xsListing) = const_val_q2_one + (
  if x 
    then boolLength xsListing 
    else boolLength xsListing)

------------------------------------------------------------------------------
-- Ex 3: Define the function validate which, given a predicate and a
-- value, evaluates to the value. However, validate should also force the
-- result of `predicate value`, even though it is not used.
--
-- Examples:
--   validate even 3               ==>  3
--   validate odd 3                ==>  3
--   validate undefined 3          ==>  an error!
--   validate (\x -> undefined) 3  ==>  an error!

validate :: (a -> Bool) -> a -> a
validate predicate value =
    let result = predicate value
    in if result
        then value
        else value 

------------------------------------------------------------------------------
-- Ex 4: Even though we can't implement the generic seq function
-- ourselves, we can implement it manually for specific datatypes.
--
-- The type class MySeq contains the method myseq which is supposed to
-- work like the built-in seq function. Implement the given MySeq
-- instances.
--
-- Just like in the course material, we use the special value
-- `undefined` here to illustrate what myseq evaluates. The tests for
-- this exercise also use undefined.
--
-- Examples:
--   myseq True  0 ==> 0
--   myseq ((\x -> x) True) 0 ==> 0
--   myseq (undefined :: Bool) 0
--     ==> *** Exception: Prelude.undefined
--   myseq (3::Int) True ==> True
--   myseq (undefined::Int) True
--     ==> *** Exception: Prelude.undefined
--   myseq [1,2] 'z' ==> 'z'
--   myseq [undefined] 'z' ==> 'z'           -- [undefined] is in WHNF
--   myseq (1:undefined) 'z' ==> 'z'         -- 1:undefined is in WHNF
--   myseq (undefined:[2,3]) 'z' ==> 'z'     -- undefined:[2,3] is in WHNF
--   myseq [1..] 'z' ==> 'z'
--   myseq (undefined::[Int])
--     ==> *** Exception: Prelude.undefined

const_val_q4_True = True
const_val_q4_False = False
const_val_q4_zero = 0
const_val_q4_one = 1
const_val_q4_empty_list = []

returnVal :: Int -> Int
returnVal a = a

class MySeq a where
  myseq :: a -> b -> b

instance MySeq Bool where
  myseq True b = b
  myseq False b = b

instance MySeq Int where
  myseq 0 b = b
  myseq yVal b = b

instance MySeq [a] where
  myseq [] b = b
  myseq (xVal : xsListing) b = b