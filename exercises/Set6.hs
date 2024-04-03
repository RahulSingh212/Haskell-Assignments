-- Exercise set 6: defining classes and instances

module Set6 where

import Mooc.Todo
import Data.Char (toLower)

------------------------------------------------------------------------------
-- Ex 1: define an Eq instance for the type Country below. You'll need
-- to use pattern matching.

const_val_q1_zero = 0
const_val_q1_one = 1
const_val_q1_True = True
const_val_q1_False = False

data Country = Finland | Switzerland | Norway
  deriving Show

-- Function to check if two countries are eq
checkCountryEquality Finland Finland = const_val_q1_True
checkCountryEquality Switzerland Switzerland = const_val_q1_True
checkCountryEquality Norway Norway = const_val_q1_True
checkCountryEquality extra1 extra2 = const_val_q1_False

instance Eq Country where
  (==) = checkCountryEquality

------------------------------------------------------------------------------
-- Ex 2: implement an Ord instance for Country so that
--   Finland <= Norway <= Switzerland
--
-- Remember minimal complete definitions!

data CountryRank = Low | Medium | High
  deriving (Eq, Ord, Show)

countryRanker :: Country -> CountryRank
countryRanker Finland = Low
countryRanker Norway = Medium
countryRanker Switzerland = High

instance Ord Country where
  compare c1 c2 = compare (countryRanker c1) (countryRanker c2)

------------------------------------------------------------------------------
-- Ex 3: Implement an Eq instance for the type Name which contains a String.
-- The Eq instance should ignore capitalization.
--
-- Hint: use the function Data.Char.toLower that has been imported for you.
--
-- Examples:
--   Name "Pekka" == Name "pekka"   ==> True
--   Name "Pekka!" == Name "pekka"  ==> False

data Name = Name String deriving Show

toLowerCaseString :: String -> String
toLowerCaseString = map toLower

stringsEqualIgnoreCase :: String -> String -> Bool
stringsEqualIgnoreCase str1 str2 = toLowerCaseString str1 == toLowerCaseString str2

instance Eq Name where
  (==) (Name a) (Name b) = stringsEqualIgnoreCase a b

------------------------------------------------------------------------------
-- Ex 4: here is a list type parameterized over the type it contains.
-- Implement an instance "Eq (List a)" that compares the lists element
-- by element.
--
-- Note how the instance needs an Eq a constraint. What happens if you
-- remove it?

const_val_q4_zero = 0
const_val_q4_one = 1
const_val_q4_True = True
const_val_q4_False = False

data List a = Empty | LNode a (List a)
  deriving Show

listEqual :: Eq a => List a -> List a -> Bool
listEqual Empty Empty = const_val_q4_True
listEqual (LNode x_val xsListing) (LNode y_val ysListing) = x_val == y_val && listEqual xsListing ysListing
listEqual extra1 extra2 = const_val_q4_False

instance Eq a => Eq (List a) where
  (==) = listEqual

------------------------------------------------------------------------------
-- Ex 5: below you'll find two datatypes, Egg and Milk. Implement a
-- type class Price, containing a function price. The price function
-- should return the price of an item.
--
-- The prices should be as follows:
-- * chicken eggs cost 20
-- * chocolate eggs cost 30
-- * milk costs 15 per liter
--
-- Example:
--   price ChickenEgg  ==>  20

data Egg = ChickenEgg | ChocolateEgg
  deriving Show
data Milk = Milk Int -- amount in litres
  deriving Show

------------------------------------------------------------------------------
-- Ex 6: define the necessary instance hierarchy in order to be able
-- to compute these:
--
-- price (Just ChickenEgg) ==> 20
-- price [Milk 1, Milk 2]  ==> 45
-- price [Just ChocolateEgg, Nothing, Just ChickenEgg]  ==> 50
-- price [Nothing, Nothing, Just (Milk 1), Just (Milk 2)]  ==> 45

const_val_q6_zero = 0
const_val_q6_one = 1
const_val_q6_Fifteen = 15
const_val_q6_Twenty = 20
const_val_q6_Thirty = 30
const_val_q6_True = True
const_val_q6_False = False

class Price a where
  price :: a -> Int

instance Price a => Price (Maybe a) where
  price (Just x) = price x
  price Nothing = const_val_q6_zero

instance Price a => Price [a] where
  price = sum . map price

instance Price Egg where
  price ChickenEgg   = const_val_q6_Twenty
  price ChocolateEgg = const_val_q6_Thirty

instance Price Milk where
  price (Milk litres) = const_val_q6_Fifteen * litres

------------------------------------------------------------------------------
-- Ex 7: below you'll find the datatype Number, which is either an
-- Integer, or a special value Infinite.
--
-- Implement an Ord instance so that finite Numbers compare normally,
-- and Infinite is greater than any other value.

compareNumbers :: Integer -> Integer -> Ordering
compareNumbers x_val y_val
  | x_val == y_val = EQ
  | otherwise = compare x_val y_val

data Number = Finite Integer | Infinite
  deriving (Show, Eq)

instance Ord Number where
  compare (Finite x_val) (Finite y_val) = compareNumbers x_val y_val
  compare (Finite extra) Infinite = LT
  compare Infinite (Finite extra) = GT
  compare Infinite Infinite = EQ

------------------------------------------------------------------------------
-- Ex 8: rational numbers have a numerator and a denominator that are
-- integers, usually separated by a horizontal bar or a slash:
--
--      numerator
--    -------------  ==  numerator / denominator
--     denominator
--
-- You may remember from school that two rationals a/b and c/d are
-- equal when a*d == b*c. Implement the Eq instance for rationals
-- using this definition.
--
-- You may assume in all exercises that the denominator is always
-- positive and nonzero.
--
-- Examples:
--   RationalNumber 4 5 == RationalNumber 4 5    ==> True
--   RationalNumber 12 15 == RationalNumber 4 5  ==> True
--   RationalNumber 13 15 == RationalNumber 4 5  ==> False

data RationalNumber = RationalNumber Integer Integer
  deriving Show

simplify :: RationalNumber -> RationalNumber
simplify (RationalNumber a b) = let
  gcd' = gcd a b
  in RationalNumber (a `div` gcd') (b `div` gcd')

areEqual :: RationalNumber -> RationalNumber -> Bool
areEqual (RationalNumber a b) (RationalNumber c d) = a * d == b * c

instance Eq RationalNumber where
  (==) x_val y_val = areEqual (simplify x_val) (simplify y_val)

------------------------------------------------------------------------------
-- Ex 9: implement the function simplify, which simplifies a rational
-- number by removing common factors of the numerator and denominator.
-- In other words,
--
--     ca         a
--    ----  ==>  ---
--     cb         b
--
-- As a concrete example,
--
--     12        3 * 4         4
--    ----  ==  -------  ==>  ---.
--     15        3 * 5         5
--
-- Hint: Remember the function gcd?

-- simplify :: RationalNumber -> RationalNumber
-- simplify p = todo

------------------------------------------------------------------------------
-- Ex 10: implement the typeclass Num for RationalNumber. The results
-- of addition and multiplication must be simplified.
--
-- Reminders:
--   * negate x is 0-x
--   * abs is absolute value
--   * signum is -1, +1 or 0 depending on the sign of the input
--
-- Examples:
--   RationalNumber 1 3 + RationalNumber 1 6 ==> RationalNumber 1 2
--   RationalNumber 1 3 * RationalNumber 3 1 ==> RationalNumber 1 1
--   negate (RationalNumber 2 3)             ==> RationalNumber (-2) 3
--   fromInteger 17 :: RationalNumber        ==> RationalNumber 17 1
--   abs (RationalNumber (-3) 2)             ==> RationalNumber 3 2
--   signum (RationalNumber (-3) 2)          ==> RationalNumber (-1) 1
--   signum (RationalNumber 0 2)             ==> RationalNumber 0 1

const_val_q10_zero = 0
const_val_q10_one = 1
const_val_q10_minus_one = -1
const_val_q10_True = True
const_val_q10_False = False

instance Num RationalNumber where
  (RationalNumber n1 d1) + (RationalNumber n2 d2) 
    = simplifyAdd (RationalNumber n1 d1) (RationalNumber n2 d2)
  
  (RationalNumber n1 d1) * (RationalNumber n2 d2) 
    = simplifyMul (RationalNumber n1 d1) (RationalNumber n2 d2)
  
  abs = absImplementation
  signum = signumImplementation
  fromInteger x = RationalNumber x 1
  negate (RationalNumber n d) = RationalNumber (-1*n) d

simplifyAdd :: RationalNumber -> RationalNumber -> RationalNumber
simplifyAdd (RationalNumber n1 d1) (RationalNumber n2 d2) = simplify (RationalNumber (n1 * d2 + n2 * d1) (d1 * d2))

simplifyMul :: RationalNumber -> RationalNumber -> RationalNumber
simplifyMul (RationalNumber n1 d1) (RationalNumber n2 d2) = simplify (RationalNumber (n1 * n2) (d1 * d2))

absImplementation :: RationalNumber -> RationalNumber
absImplementation (RationalNumber n d) = RationalNumber (abs n) (abs d)

signumImplementation :: RationalNumber -> RationalNumber
signumImplementation (RationalNumber n d)
  | n == const_val_q10_zero = 0
  | n > const_val_q10_zero = const_val_q10_one
  | otherwise = const_val_q10_minus_one

------------------------------------------------------------------------------
-- Ex 11: a class for adding things. Define a class Addable with a
-- constant `zero` and a function `add`. Define instances of Addable
-- for Integers and lists. Numbers are added with the usual addition,
-- while lists are added by catenating them. Pick a value for `zero`
-- such that: `add zero x == x`
--
-- Examples:
--   add 1 2                ==>  3
--   add 1 zero             ==>  1
--   add [1,2] [3,4]        ==>  [1,2,3,4]
--   add zero [True,False]  ==>  [True,False]

const_val_q11_zero = 0
const_val_q11_one = 1
const_val_q11_True = True
const_val_q11_False = False
const_val_q11_empty_list = []

class Addable a where
  zero :: a
  add :: a -> a -> a

instance Addable Integer where
  zero = const_val_q11_zero
  add = (+)

instance Addable [a] where
  zero = const_val_q11_empty_list
  add = (++)

instance Addable a => Addable (Maybe a) where
  zero = Nothing
  add = addMaybe

addMaybe :: Addable a => Maybe a -> Maybe a -> Maybe a
addMaybe Nothing y_val = y_val
addMaybe x_val Nothing = x_val
addMaybe (Just x_val) (Just y_val) = Just (add x_val y_val)

------------------------------------------------------------------------------
-- Ex 12: cycling. Implement a type class Cycle that contains a
-- function `step` that cycles through the values of the type.
-- Implement instances for Color and Suit that work like this:
--
--   step Red ==> Green
--   step Green ==> Blue
--   step Blue ==> Red
--
-- The suit instance should cycle suits in the order Club, Spade,
-- Diamond, Heart, Club.
--
-- Also add a function `stepMany` to the class and give it a default
-- implementation using `step`. The function `stepMany` should take
-- multiple (determined by an Int argument) steps like this:
--
--   stepMany 2 Club ==> Diamond
--   stepMany 3 Diamond ==> Spade
--
-- The tests will test the Cycle class and your default implementation
-- of stepMany by adding an instance like this:
--
--    instance Cycle Int where
--      step = succ

data Color = Red | Green | Blue
  deriving (Show, Eq, Enum, Bounded)

data Suit = Club | Spade | Diamond | Heart
  deriving (Show, Eq, Enum, Bounded)

class Enum a => Cycle a where
  step :: a -> a
  stepMany :: Int -> a -> a
  stepMany n x = iterate step x !! n

instance Cycle Color where
  step = cycleColors
  stepMany n x = iterate cycleColors x !! n

instance Cycle Suit where
  step = cycleSuits
  stepMany n x = iterate cycleSuits x !! n

cycleColors :: Color -> Color
cycleColors Red = Green
cycleColors Green = Blue
cycleColors Blue = Red

cycleSuits :: Suit -> Suit
cycleSuits Club = Spade
cycleSuits Spade = Diamond
cycleSuits Diamond = Heart
cycleSuits Heart = Club

