module Set16b where

import Mooc.Todo
import Examples.Phantom
import Data.Char (toUpper)

------------------------------------------------------------------------------
-- Ex 1: Define a constant pounds with type Money GBP and a value of
-- 3. The type Money is imported from Example.Phantom but you'll need
-- to introduce GBP yourself.

const_val_q1_True = True
const_val_q1_False = False
const_val_q1_zero = 0
const_val_q1_one = 1
const_val_q1_two = 2
const_val_q1_three = 3
const_val_q1_empty_list = []

data GBP
pounds :: Money GBP
pounds = Money const_val_q1_three

------------------------------------------------------------------------------
-- Ex 2: Implement composition for Rates. Give composeRates a
-- restricted type so that the currencies are tracked correctly.
--
-- Examples:
--   composeRates (Rate 1.5) (Rate 1.25) ==> Rate 1.875
--   composeRates eurToUsd usdToChf :: Rate EUR CHF
--   composeRates eurToUsd (invert eurToUsd) :: Rate EUR EUR
--   composeRates eurToUsd eurToUsd :: type error!
--   composeRates eurToUsd :: Rate USD to -> Rate EUR to

-- For testing
usdToChf :: Rate USD CHF
usdToChf = Rate 1.11

getMultiplication :: Int -> Int -> Int
getMultiplication a b = a * b

composeRates :: Rate from to1 -> Rate to1 to2 -> Rate from to2
composeRates (Rate r1) (Rate r2) = Rate (r1 * r2)

------------------------------------------------------------------------------
-- Ex 3: Tracking first, last and full names with phantom types. The
-- goal is to have the types:
--  * Name First - for first names
--  * Name Last - for last names
--  * Name Full - for full names
--
-- In this exercise, you should define the phantom types First, Last
-- and Full, and the parameterised type Name. Then implement the
-- functions fromName, toFirst and toLast. Give the functions the
-- commented-out types
--
-- Examples:
--  fromName (toFirst "bob") ==> "bob"
--  fromName (toLast "smith") ==> "smith"
--  toFirst "bob" :: Name First
--  toLast "smith" :: Name Last

data First
data Last
data Full

data Name a = Name String

fromName :: Name a -> String
fromName (Name s) = s

toFirst :: String -> Name First
toFirst = Name

toLast :: String -> Name Last
toLast = Name

------------------------------------------------------------------------------
-- Ex 4: Implement the functions capitalize and toFull.
--
-- toFull should combine a first and a last name into a full name. Give
-- toFull the correct type (see examples below).
--
-- capitalize should capitalize the first letter of a name. Give
-- capitalize the correct type (see examples below).
--
-- Examples:
--  toFull (toFirst "bob") (toLast "smith") :: Name Full
--  fromName (toFull (toFirst "bob") (toLast "smith"))
--    ==> "bob smith"
--  capitalize (toFirst "bob") :: Name First
--  fromName (capitalize (toFirst "bob")) ==> "Bob"
--  capitalize (toLast "smith") :: Name Last
--  fromName (capitalize (toLast "smith")) ==> "Smith"

const_val_q4_True = True
const_val_q4_False = False
const_val_q4_zero = 0
const_val_q4_one = 1
const_val_q4_empty_list = []
const_val_q4_space = " "

capitalize :: Name a -> Name a
capitalize (Name (x:xs)) = Name (toUpper x : xs)

toFull :: Name First -> Name Last -> Name Full
toFull (Name first) (Name last) = Name (first ++ const_val_q4_space ++ last)

------------------------------------------------------------------------------
-- Ex 5: Type classes can let you write code that handles different
-- phantom types differently. Define instances for the Render type
-- class such that:
--
--  render (Money 1.0 :: Money EUR) ==> "1.0e"
--  render (Money 1.0 :: Money USD) ==> "$1.0"
--  render (Money 1.0 :: Money CHF) ==> "1.0chf"

const_val_q5_True = True
const_val_q5_False = False
const_val_q5_zero = 0
const_val_q5_one = 1
const_val_q5_two = 2
const_val_q5_three = 3
const_val_q5_empty_list = []
const_val_q5_str1 = "e"
const_val_q5_str2 = "$"
const_val_q5_str3 = "chf"

class Render currency where
  render :: Money currency -> String

instance Render EUR where
  render (Money amount) = show amount ++ const_val_q5_str1

instance Render USD where
  render (Money amount) = const_val_q5_str2 ++ show amount

instance Render CHF where
  render (Money amount) = show amount ++ const_val_q5_str3