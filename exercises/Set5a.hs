-- Exercise set 5a
--
-- * defining algebraic datatypes
-- * recursive datatypes

module Set5a where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Define the type Vehicle that has four constructors: Bike,
-- Bus, Tram and Train.
--
-- The constructors don't need any fields.

data Vehicle = Bike | Bus | Tram | Train deriving Show

------------------------------------------------------------------------------
-- Ex 2: Define the type BusTicket that can represent values like these:
--  - SingleTicket
--  - MonthlyTicket "January"
--  - MonthlyTicket "December"

data BusTicket = SingleTicket | MonthlyTicket String deriving Show

------------------------------------------------------------------------------
-- Ex 3: Here's the definition for a datatype ShoppingEntry that
-- represents an entry in a shopping basket. It has an item name (a
-- String), an item price (a Double) and a count (an Int). You'll also
-- find two examples of ShoppingEntry values.
--
-- Implement the functions totalPrice and buyOneMore below.

const_val_q3_str_Apple = "Apple"
const_val_q3_str_Banana = "Banana"
const_val_q3_one = 1

data ShoppingEntry = MkShoppingEntry String Double Int
  deriving Show

threeApples :: ShoppingEntry
threeApples = MkShoppingEntry const_val_q3_str_Apple 0.5 3

twoBananas :: ShoppingEntry
twoBananas = MkShoppingEntry const_val_q3_str_Banana 1.1 2

-- totalPrice should return the total price for an entry
--
-- Hint: you'll probably need fromIntegral to convert the Int into a
-- Double
--
-- Examples:
--   totalPrice threeApples  ==> 1.5
--   totalPrice twoBananas   ==> 2.2

totalPrice :: ShoppingEntry -> Double
totalPrice (MkShoppingEntry exVal price cntValue) = price * fromIntegral cntValue

-- buyOneMore should increment the count in an entry by one
--
-- Example:
--   buyOneMore twoBananas    ==> MkShoppingEntry "Banana" 1.1 3

buyOneMore :: ShoppingEntry -> ShoppingEntry
buyOneMore (MkShoppingEntry name price cntValue) 
  = MkShoppingEntry name price (cntValue + const_val_q3_one)

------------------------------------------------------------------------------
-- Ex 4: define a datatype Person, which should contain the age (an
-- Int) and the name (a String) of a person.
--
-- Also define a Person value fred, and the functions getAge, getName,
-- setAge and setName (see below).

const_val_q4_str_Fred = "Fred"
const_val_q4_90 = 90

data Person = Person { personName :: String, personAge :: Int }
  deriving Show

fred :: Person
fred = Person { personName = const_val_q4_str_Fred, personAge = 90 }

getName :: Person -> String
getName = personName

getAge :: Person -> Int
getAge = personAge

setName :: String -> Person -> Person
setName newName person = person { personName = newName }

setAge :: Int -> Person -> Person
setAge newAge person = person { personAge = newAge }

------------------------------------------------------------------------------
-- Ex 5: define a datatype Position which contains two Int values, x
-- and y. Also define the functions below for operating on a Position.
--
-- Examples:
--   getY (up (up origin))    ==> 2
--   getX (up (right origin)) ==> 1

const_val_q5_zero = 0
const_val_q5_one = 1

data Position = Position { xPos :: Int, yPos :: Int }
  deriving (Show)

origin :: Position
origin = Position { xPos = const_val_q5_zero, yPos = const_val_q5_zero }

getX :: Position -> Int
getX = xPos

getY :: Position -> Int
getY = yPos

up :: Position -> Position
up pos = pos { yPos = yPos pos + const_val_q5_one }

right :: Position -> Position
right pos = pos { xPos = xPos pos + const_val_q5_one }

------------------------------------------------------------------------------
-- Ex 6: Here's a datatype that represents a student. A student can
-- either be a freshman, a nth year student, or graduated.

const_val_q6_zero  = 0
const_val_q6_one   = 1
const_val_q6_seven = 7

data Student = Freshman | NthYear Int | Graduated
  deriving (Show,Eq)

-- Implement the function study, which changes a Freshman into a 1st
-- year student, a 1st year student into a 2nd year student, and so
-- on. A 7th year student gets changed to a graduated student. A
-- graduated student stays graduated even if he studies.

study :: Student -> Student
study Freshman = NthYear const_val_q6_one
study (NthYear n) 
  = if n < const_val_q6_seven 
    then NthYear (n + const_val_q6_one) 
    else Graduated
study Graduated = Graduated

------------------------------------------------------------------------------
-- Ex 7: define a datatype UpDown that represents a counter that can
-- either be in increasing or decreasing mode. Also implement the
-- functions zero, toggle, tick and get below.
--
-- NB! Define _two_ constructors for your datatype (feel free to name the
-- constructors however you want)
--
-- Examples:
--
-- get (tick zero)
--   ==> 1
-- get (tick (tick zero))
--   ==> 2
-- get (tick (tick (toggle (tick zero))))
--   ==> -1

const_val_q7_zero  = 0
const_val_q7_one   = 1

data UpDown = Increasing Int | Decreasing Int
  deriving (Show, Eq)

zero :: UpDown
zero = Increasing const_val_q7_zero

get :: UpDown -> Int
get (Increasing nthValue) = nthValue
get (Decreasing nthValue) = nthValue

tick :: UpDown -> UpDown
tick (Increasing nthValue) = Increasing (nthValue + const_val_q7_one)
tick (Decreasing nthValue) = Decreasing (nthValue - const_val_q7_one)

toggle :: UpDown -> UpDown
toggle (Increasing nthValue) = Decreasing nthValue
toggle (Decreasing nthValue) = Increasing nthValue

------------------------------------------------------------------------------
-- Ex 8: you'll find a Color datatype below. It has the three basic
-- colours Red, Green and Blue, and two color transformations, Mix and
-- Invert.
--
-- Mix means the average of the two colors in each rgb channel.
--
-- Invert means subtracting all rgb values from 1.
--
-- Implement the function rgb :: Color -> [Double] that returns a list
-- of length three that represents the rgb value of the given color.
--
-- Examples:
--
-- rgb Red   ==> [1,0,0]
-- rgb Green ==> [0,1,0]
-- rgb Blue  ==> [0,0,1]
--
-- rgb (Mix Red Green)                    ==> [0.5,0.5,0]
-- rgb (Mix Red (Mix Red Green))          ==> [0.75,0.25,0]
-- rgb (Invert Red)                       ==> [0,1,1]
-- rgb (Invert (Mix Red (Mix Red Green))) ==> [0.25,0.75,1]
-- rgb (Mix (Invert Red) (Invert Green))  ==> [0.5,0.5,1]

const_val_q8_zero  = 0
const_val_q8_one   = 1
const_val_q8_two   = 2
const_val_Red_List   = [const_val_q8_one, const_val_q8_zero, const_val_q8_zero]
const_val_Green_List = [const_val_q8_zero, const_val_q8_one, const_val_q8_zero]
const_val_Blue_List  = [const_val_q8_zero, const_val_q8_zero, const_val_q8_one]

data Color = Red | Green | Blue | Mix Color Color | Invert Color
  deriving Show

average :: Double -> Double -> Double
average x y = (x + y) / const_val_q8_two

mixColors :: [Double] -> [Double] -> [Double]
mixColors [] [] = []
mixColors (x_val:xsListing) (y_val:ysListing) = average x_val y_val : mixColors xsListing ysListing

invertColors :: [Double] -> [Double]
invertColors = map (\x_val -> const_val_q8_one - x_val)

rgb :: Color -> [Double]
rgb Red   = const_val_Red_List
rgb Green = const_val_Green_List
rgb Blue  = const_val_Blue_List
rgb (Mix c1 c2) = mixColors (rgb c1) (rgb c2)
rgb (Invert c) = invertColors (rgb c)

------------------------------------------------------------------------------
-- Ex 9: define a parameterized datatype OneOrTwo that contains one or
-- two values of the given type. The constructors should be called One and Two.
--
-- Examples:
--   One True         ::  OneOrTwo Bool
--   Two "cat" "dog"  ::  OneOrTwo String

data OneOrTwo a = One a | Two a a
  deriving Show

------------------------------------------------------------------------------
-- Ex 10: define a recursive datatype KeyVals for storing a set of
-- key-value pairs. There should be two constructors: Empty and Pair.
--
-- Empty represents an empty collection. It should have no fields.
--
-- Pair should have three fields, one for the key, one for the value,
-- and one for the rest of the collection (of type KeyVals)
--
-- The KeyVals datatype is parameterized by the key type k and
-- the value type v.
--
-- For example:
--
--  Pair "cat" True (Pair "dog" False Empty)  ::  KeyVals String Bool
--
-- Also define the functions toList and fromList that convert between
-- KeyVals and lists of pairs.

const_val_q10_zero  = 0
const_val_q10_one   = 1
const_val_q10_two   = 2
const_val_q10_empty_list   = []

data KeyVals k v = Empty | Pair k v (KeyVals k v)
  deriving Show

toList :: KeyVals k v -> [(k, v)]
toList Empty = const_val_q10_empty_list
toList (Pair k v rest) = (k, v) : toList rest

fromList :: [(k, v)] -> KeyVals k v
fromList [] = Empty
fromList ((k, v):kvs) = Pair k v (fromList kvs)

------------------------------------------------------------------------------
-- Ex 11: The data type Nat is the so called Peano
-- representation for natural numbers. Define functions fromNat and
-- toNat that convert natural numbers to Ints and vice versa.
--
-- Examples:
--   fromNat (PlusOne (PlusOne (PlusOne Zero)))  ==>  3
--   toNat 3    ==> Just (PlusOne (PlusOne (PlusOne Zero)))
--   toNat (-3) ==> Nothing
--

const_val_q11_zero  = 0
const_val_q11_one   = 1
const_val_q11_two   = 2
const_val_q11_empty_list   = []

data Nat = Zero | PlusOne Nat
  deriving (Show, Eq)

fromNat :: Nat -> Int
fromNat nat = extractFromNat nat 0
  where
    extractFromNat :: Nat -> Int -> Int
    extractFromNat Zero acc = acc
    extractFromNat (PlusOne nthValue) acc = extractFromNat nthValue (acc + const_val_q11_one)

toNat :: Int -> Maybe Nat
toNat z_val
  | z_val < const_val_q11_zero = Nothing
  | otherwise = Just (toNatHelper z_val)
  where
    toNatHelper :: Int -> Nat
    toNatHelper 0 = Zero
    toNatHelper nthValue = constructNat nthValue
      where
        constructNat :: Int -> Nat
        constructNat 0 = Zero
        constructNat nthValue = PlusOne (toNatHelper (nthValue - 1))

------------------------------------------------------------------------------
-- Ex 12: While pleasingly simple in its definition, the Nat datatype is not
-- very efficient computationally. Instead of the unary Peano natural numbers,
-- computers use binary numbers.
--
-- Binary numbers are like decimal numbers, except that binary numbers have
-- only two digits (called bits), 0 and 1. The table below gives some
-- examples:
--
--   decimal | binary
--   --------+-------
--         0 |      0
--         1 |      1
--         2 |     10
--         7 |    111
--        44 | 101100
--
-- For allowing arbitrarily long binary numbers, our representation, the
-- datatype Bin, includes a special End constructor for denoting the end of
-- the binary number. In order to make computation with Bin easier, the bits
-- are represented in increasing order by significance (i.e. "backwards").
-- Consider the Bin numbers O (I (I End)), representing 110 in binary or
-- 6 in decimal, and I (I (O End)) that represents 011 in binary or 3 in
-- decimal. The most significant (last) bit, the bit I, of O (I (I End)) is
-- greater than the bit O, which is the most significant bit of I (I (O End)).
-- Therefore, O (I (I End)) is greater than I (I (O End)).
--
-- Your task is to write functions prettyPrint, fromBin, and toBin that
-- convert Bin to human-readable string, Bin to Int, and Int to Bin
-- respectively.
--
-- Examples:
--   prettyPrint End                     ==> ""
--   prettyPrint (O End)                 ==> "0"
--   prettyPrint (I End)                 ==> "1"
--   prettyPrint (O (O (I (O (I End))))) ==> "10100"
--   map fromBin [O End, I End, O (I End), I (I End), O (O (I End)),
--                  I (O (I End))]
--     ==> [0, 1, 2, 3, 4, 5]
--   fromBin (I (I (O (O (I (O (I (O End)))))))) ==> 83
--   fromBin (I (I (O (O (I (O (I End)))))))     ==> 83
--   map toBin [0..5] ==>
--     [O End,I End,O (I End),I (I End),O (O (I End)),I (O (I End))]
--   toBin 57 ==> I (O (O (I (I (I End)))))
--
-- Challenge: Can you implement toBin by directly converting its input into a
-- sequence of bits instead of repeatedly applying inc?
--

data Bin = End | O Bin | I Bin
  deriving (Show, Eq)

-- Convert a binary number to an integer
fromBin :: Bin -> Int
fromBin bin = fromBinHelper bin 0
  where
    fromBinHelper :: Bin -> Int -> Int
    fromBinHelper End acc     = acc
    fromBinHelper (O b) acc   = fromBinHelper b (acc * 2)
    fromBinHelper (I b) acc   = fromBinHelper b (acc * 2 + 1)

-- Convert an integer to a binary number
toBin :: Int -> Bin
toBin n
  | n < 0     = error "Negative numbers not supported"
  | n == 0    = O End
  | otherwise = toBinHelper n
  where
    toBinHelper :: Int -> Bin
    toBinHelper 0 = End
    toBinHelper n
      | n `mod` 2 == 0 = O (toBinHelper (n `div` 2))
      | otherwise      = I (toBinHelper (n `div` 2))

-- Convert a binary number to a human-readable string
prettyPrint :: Bin -> String
prettyPrint End     = ""
prettyPrint (O b)   = prettyPrint b ++ "0"
prettyPrint (I b)   = prettyPrint b ++ "1"