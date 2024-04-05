-- Exercise set 7

module Set7 where

import Mooc.Todo
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid
import Data.Semigroup
import Data.List.NonEmpty ((<|))

------------------------------------------------------------------------------
-- Ex 1: you'll find below the types Time, Distance and Velocity,
-- which represent time, distance and velocity in seconds, meters and
-- meters per second.
--
-- Implement the functions below.

data Distance = Distance Double
  deriving (Show,Eq)

data Time = Time Double
  deriving (Show,Eq)

data Velocity = Velocity Double
  deriving (Show,Eq)

-- velocity computes a velocity given a distance and a time
velocity :: Distance -> Time -> Velocity
velocity (Distance d) (Time t) = Velocity (d / t)

-- travel computes a distance given a velocity and a time
travel :: Velocity -> Time -> Distance
travel (Velocity v) (Time t) = Distance (v * t)

------------------------------------------------------------------------------
-- Ex 2: let's implement a simple Set datatype. A Set is a list of
-- unique elements. The set is always kept ordered.
--
-- Implement the functions below. You might need to add class
-- constraints to the functions' types.
--
-- Examples:
--   member 'a' (Set ['a','b','c'])  ==>  True
--   add 2 (add 3 (add 1 emptySet))  ==>  Set [1,2,3]
--   add 1 (add 1 emptySet)  ==>  Set [1]

data Set a = Set [a]
  deriving (Show,Eq)

emptySet :: Set a
emptySet = Set []

member :: Eq a => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

add :: Ord a => a -> Set a -> Set a
add x (Set xs) = Set (insertInOrder x xs)

insertInOrder :: Ord a => a -> [a] -> [a]
insertInOrder x_val [] = [x_val]
insertInOrder x_val (y_val:ysListing)
  | x_val < y_val     = x_val : y_val : ysListing
  | x_val == y_val   = y_val : ysListing
  | otherwise = y_val : insertInOrder x_val ysListing

------------------------------------------------------------------------------
-- Ex 3: a state machine for baking a cake. The type Event represents
-- things that can happen while baking a cake. The type State is meant
-- to represent the states a cake can be in.
--
-- Your job is to
--
--  * add new states to the State type
--  * and implement the step function
--
-- so that they have the following behaviour:
--
--  * Baking starts in the Start state
--  * A successful cake (reperesented by the Finished value) is baked
--    by first adding eggs, then adding flour and sugar (flour and
--    sugar can be added in which ever order), then mixing, and
--    finally baking.
--  * If the order of Events differs from this, the result is an Error cake.
--    No Events can save an Error cake.
--  * Once a cake is Finished, it stays Finished even if additional Events happen.
--
-- The function bake just calls step repeatedly. It's used for the
-- examples below. Don't modify it.
--
-- Examples:
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake]  ==>  Finished
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake,AddSugar,Mix]  ==> Finished
--   bake [AddFlour]  ==>  Error
--   bake [AddEggs,AddFlour,Mix]  ==>  Error

data Event = AddEggs | AddFlour | AddSugar | Mix | Bake
  deriving (Eq, Show)

data State = Start | AddingEggs | AddingFlour | SugarResults | Mixing | BakeResults | Finished | Error
  deriving (Eq, Show)

step :: State -> Event -> State
step Error _ = Error
step Finished _ = Finished
step Start AddEggs = AddingEggs
step AddingEggs AddFlour = AddingFlour
step AddingEggs AddSugar = SugarResults
step AddingFlour AddSugar = Mixing
step SugarResults AddFlour = Mixing
step Mixing Mix = BakeResults
step BakeResults Bake = Finished
step extra1 extra2 = Error

bake :: [Event] -> State
bake events = go Start events
  where go state [] = state
        go state (e:es) = go (step state e) es

------------------------------------------------------------------------------
-- Ex 4: remember how the average function from Set4 couldn't really
-- work on empty lists? Now we can reimplement average for NonEmpty
-- lists and avoid the edge case.
--
-- PS. The Data.List.NonEmpty type has been imported for you
--
-- Examples:
--   average (1.0 :| [])  ==>  1.0
--   average (1.0 :| [2.0,3.0])  ==>  2.0

const_val_q4_zero = 0
const_val_q4_one = 1

average :: Fractional a => NonEmpty a -> a
average (x_val :| xsListing) 
  = sumList (x_val : xsListing) / fromIntegral (lengthList (x_val : xsListing))

sumList :: Num a => [a] -> a
sumList = foldr (+) 0

lengthList :: [a] -> Int
lengthList = foldr (\_ acc -> acc + const_val_q4_one) const_val_q4_zero

------------------------------------------------------------------------------
-- Ex 5: reverse a NonEmpty list.
--
-- PS. The Data.List.NonEmpty type has been imported for you

reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty (x :| xs) = go (x :| []) xs
  where
    go acc [] = acc
    go acc (y:ys) = go (y <| acc) ys

------------------------------------------------------------------------------
-- Ex 6: implement Semigroup instances for the Distance, Time and
-- Velocity types from exercise 1. The instances should perform
-- addition.
--
-- When you've defined the instances you can do things like this:
--
-- velocity (Distance 50 <> Distance 10) (Time 1 <> Time 2)
--    ==> Velocity 20

instance Semigroup Distance where
  (Distance x_val) <> (Distance y_val) = Distance (x_val + y_val)

instance Semigroup Time where
  (Time x_val) <> (Time y_val) = Time (x_val + y_val)

instance Semigroup Velocity where
  (Velocity x_val) <> (Velocity y_val) = Velocity (x_val + y_val)

------------------------------------------------------------------------------
-- Ex 7: implement a Monoid instance for the Set type from exercise 2.
-- The (<>) operation should be the union of sets.
--
-- What's the right definition for mempty?
--
-- What are the class constraints for the instances?

instance Ord a => Semigroup (Set a) where
  (Set xsListing) <> (Set ysListing) = Set (merge xsListing ysListing)
    where
      merge :: Ord a => [a] -> [a] -> [a]
      merge [] ysListing = ysListing
      merge xsListing [] = xsListing
      merge (x_val:xsListing) (y_val:ysListing)
        | x_val < y_val     = x_val : merge xsListing (y_val:ysListing)
        | x_val == y_val    = x_val : merge xsListing ysListing
        | otherwise = y_val : merge (x_val:xsListing) ysListing

instance Ord a => Monoid (Set a) where
  mempty = emptySet
    where
      emptySet = Set []

  mappend = (<>)

------------------------------------------------------------------------------
-- Ex 8: below you'll find two different ways of representing
-- calculator operations. The type Operation1 is a closed abstraction,
-- while the class Operation2 is an open abstraction.
--
-- Your task is to add:
--  * a multiplication case to Operation1 and Operation2
--    (named Multiply1 and Multiply2, respectively)
--  * functions show1 and show2 that render values of
--    Operation1 and Operation2 to strings
--
-- Examples:
--   compute1 (Multiply1 2 3) ==> 6
--   compute2 (Multiply2 2 3) ==> 6
--   show1 (Add1 2 3) ==> "2+3"
--   show1 (Multiply1 4 5) ==> "4*5"
--   show2 (Subtract2 2 3) ==> "2-3"
--   show2 (Multiply2 4 5) ==> "4*5"

const_val_q8_zero = 0
const_val_q8_one = 1
const_val_q8_str_plus = "+"
const_val_q8_str_minus = "-"
const_val_q8_str_multiple = "*"

data Operation1 = Add1 Int Int
                | Subtract1 Int Int
                | Multiply1 Int Int
  deriving Show

compute1 :: Operation1 -> Int
compute1 (Add1 i_idx j_idx) = i_idx + j_idx
compute1 (Subtract1 i_idx j_idx) = i_idx - j_idx
compute1 (Multiply1 i_idx j_idx) = i_idx * j_idx

show1 :: Operation1 -> String
show1 (Add1 i_idx j_idx) = show i_idx ++ const_val_q8_str_plus ++ show j_idx
show1 (Subtract1 i_idx j_idx) = show i_idx ++ const_val_q8_str_minus ++ show j_idx
show1 (Multiply1 i_idx j_idx) = show i_idx ++ const_val_q8_str_multiple ++ show j_idx

data Add2 = Add2 Int Int
  deriving Show
data Subtract2 = Subtract2 Int Int
  deriving Show
data Multiply2 = Multiply2 Int Int
  deriving Show

class Operation2 op where
  compute2 :: op -> Int
  show2 :: op -> String

instance Operation2 Add2 where
  compute2 (Add2 i_idx j_idx) = i_idx + j_idx
  show2 (Add2 i_idx j_idx) = show i_idx ++ const_val_q8_str_plus ++ show j_idx

instance Operation2 Subtract2 where
  compute2 (Subtract2 i_idx j_idx) = i_idx - j_idx
  show2 (Subtract2 i_idx j_idx) = show i_idx ++ const_val_q8_str_minus ++ show j_idx

instance Operation2 Multiply2 where
  compute2 (Multiply2 i_idx j_idx) = i_idx * j_idx
  show2 (Multiply2 i_idx j_idx) = show i_idx ++ const_val_q8_str_multiple ++ show j_idx

------------------------------------------------------------------------------
-- Ex 9: validating passwords. Below you'll find a type
-- PasswordRequirement describing possible requirements for passwords.
--
-- Implement the function passwordAllowed that checks whether a
-- password is allowed.
--
-- Examples:
--   passwordAllowed "short" (MinimumLength 8) ==> False
--   passwordAllowed "veryLongPassword" (MinimumLength 8) ==> True
--   passwordAllowed "password" (ContainsSome "0123456789") ==> False
--   passwordAllowed "p4ssword" (ContainsSome "0123456789") ==> True
--   passwordAllowed "password" (DoesNotContain "0123456789") ==> True
--   passwordAllowed "p4ssword" (DoesNotContain "0123456789") ==> False
--   passwordAllowed "p4ssword" (And (ContainsSome "1234") (MinimumLength 5)) ==> True
--   passwordAllowed "p4ss" (And (ContainsSome "1234") (MinimumLength 5)) ==> False
--   passwordAllowed "p4ss" (Or (ContainsSome "1234") (MinimumLength 5)) ==> True

-- Define a new data type for password requirements
data PasswordRequirement =
    MinimumLength Int
  | ContainsSome String
  | DoesNotContain String
  | And PasswordRequirement PasswordRequirement
  | Or PasswordRequirement PasswordRequirement
  deriving Show

meetsMinimumLength :: String -> Int -> Bool
meetsMinimumLength pwd minLen = length pwd >= minLen

containsSome :: String -> String -> Bool
containsSome pwd chars = not (null (filter (`elem` chars) pwd))

doesNotContain :: String -> String -> Bool
doesNotContain pwd chars = null (filter (`elem` chars) pwd)

checkBothRequirements :: String -> PasswordRequirement -> PasswordRequirement -> Bool
checkBothRequirements pwd req1 req2 = passwordAllowed pwd req1 && passwordAllowed pwd req2

checkForEitherRequirement :: String -> PasswordRequirement -> PasswordRequirement -> Bool
checkForEitherRequirement pwd req1 req2 = passwordAllowed pwd req1 || passwordAllowed pwd req2

passwordAllowed :: String -> PasswordRequirement -> Bool
passwordAllowed pwd (MinimumLength minLen) = meetsMinimumLength pwd minLen
passwordAllowed pwd (ContainsSome chars) = containsSome pwd chars
passwordAllowed pwd (DoesNotContain chars) = doesNotContain pwd chars
passwordAllowed pwd (And req1 req2) = checkBothRequirements pwd req1 req2
passwordAllowed pwd (Or req1 req2) = checkForEitherRequirement pwd req1 req2

------------------------------------------------------------------------------
-- Ex 10: a DSL for simple arithmetic expressions with addition and
-- multiplication. Define the type Arithmetic so that it can express
-- expressions like this. Define the functions literal and operation
-- for creating Arithmetic values.
--
-- Define two interpreters for Arithmetic: evaluate should compute the
-- expression, and render should show the expression as a string.
--
-- Examples:
--   evaluate (literal 3) ==> 3
--   render   (literal 3) ==> "3"
--   evaluate (operation "+" (literal 3) (literal 4)) ==> 7
--   render   (operation "+" (literal 3) (literal 4)) ==> "(3+4)"
--   evaluate (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))
--     ==> 6
--   render   (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))
--     ==> "(3*(1+1))"
--

const_val_q10_zero = 0
const_val_q10_one = 1
const_val_q10_str_plus     = "+"
const_val_q10_str_multiply = "*"
const_val_q10_str_bOpen  = "("
const_val_q10_str_bClose = ")"
const_val_q10_str_sentence1 = "Unsupported operation"

data Arithmetic = Literal Integer | Operation String Arithmetic Arithmetic deriving Show

literal :: Integer -> Arithmetic
literal = Literal

operation :: String -> Arithmetic -> Arithmetic -> Arithmetic
operation = Operation

evaluate :: Arithmetic -> Integer
evaluate (Literal x_val) = x_val
evaluate (Operation "+" x_val y_val) = evaluate x_val + evaluate y_val
evaluate (Operation "*" x_val y_val) = evaluate x_val * evaluate y_val
evaluate (Operation extra1 extra2 extra3) = error const_val_q10_str_sentence1

render :: Arithmetic -> String
render (Literal x_val) = show x_val
render (Operation op x_val y_val) = const_val_q10_str_bOpen ++ render x_val ++ op ++ render y_val ++ const_val_q10_str_bClose