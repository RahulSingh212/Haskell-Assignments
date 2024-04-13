{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-} -- this silences an uninteresting warning

module Set13b where

import Mooc.Todo

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.IORef
import Data.List

------------------------------------------------------------------------------
-- Ex 1: implement the function ifM, that takes three monadic
-- operations. If the first of the operations returns True, the second
-- operation should be run. Otherwise the third operation should be
-- run.
--
-- Note the polymorphic `Monad m =>` type signature. Your operation
-- should work on all monads, and thus needs to be implemented with
-- Monad operations like do and >>=. Don't try to pattern match on
-- Maybes.
--
-- Examples (test is defined below):
--   In the Maybe Monad:
--     ifM (Just True) (Just '1') (Just '2')  ==>  Just '1'
--     ifM (Just False) (Just '1') (Just '2') ==>  Just '2'
--     ifM Nothing (Just '1') (Just '2')      ==>  Nothing
--     ifM (Just True) (Just '1') Nothing     ==>  Just '1'
--   In the State Monad (test is defined below):
--     runState (ifM get (return 'a') (return 'b')) False
--       ==> ('b',False)
--     runState (put 11 >> ifM test (return 'a') (return 'b')) 0
--       ==> ('b',11)
--     runState (put 9 >> ifM test (return 'a') (return 'b')) 0
--       ==> ('a',9)

const_val_q1_True = True
const_val_q1_False = False
const_val_q1_zero = 0
const_val_q1_one = 1
const_val_q1_empty_list = []
const_val_q1_str1 = ""

test :: State Int Bool
test = do
  x <- get
  return (x<10)

branch :: Monad m => Bool -> m a -> m a -> m a
branch bool opThen opElse =
  if bool then opThen else opElse

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM opBool opThen opElse = do
  bool <- opBool
  branch bool opThen opElse

------------------------------------------------------------------------------
-- Ex 2: the standard library function Control.Monad.mapM defines a
-- monadic map operation. Some examples of using it (safeDiv is defined
-- below):
--
-- mapM (safeDiv 10.0) [1.0,5.0,2.0]  =>  Just [10.0,2.0,5.0]
-- mapM (safeDiv 10.0) [1.0,0.0,2.0]  =>  Nothing
--
-- Your task is to implement the function mapM2 that works like mapM,
-- but there are two lists and the operation takes two arguments. More
-- concretely, running `mapM2 op xs ys` should run `op`, giving it the
-- first element of xs and the first element of ys. Then, it should
-- run `op` on the second elements of xs and ys, and so forth.
-- Finally, all the values produced by `op` are returned, in order, as
-- a list.
--
-- If the lists are of different length, you can stop processing them
-- once the shorter one ends.
--
-- Examples:
--  mapM2 safeDiv [6.0,10.0,12.0] [3.0,2.0,4.0]
--    ==> Just [2.0,5.0,3.0]
--  mapM2 safeDiv [6.0,10.0,12.0] [3.0,0.0,4.0]
--    ==> Nothing
--  mapM2 (\x y -> Just (x+y)) [1,2,3] [6,7]
--    ==> Just [7,9]
--  runState (mapM2 perhapsIncrement [True,False,True] [1,2,4]) 0
--    ==> ([(),(),()],5)

-- Do not change safeDiv or perhapsIncrement, they're used by the
-- examples & test outputs.

const_val_q2_True = True
const_val_q2_False = False
const_val_q2_zero = 0
const_val_q2_one = 1
const_val_q2_Nothing = Nothing
const_val_q2_empty_list = []
const_val_q2_str1 = ""

mapPairM :: Monad m => (a -> b -> m c) -> (a, b) -> m c
mapPairM op (x, y) = op x y

mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 extra1 [] extra2 = return const_val_q2_empty_list
mapM2 extra1 extra2 [] = return const_val_q2_empty_list
mapM2 op (x:xs) (y:ys) = do
  res <- mapPairM op (x, y)
  rest <- mapM2 op xs ys
  return (res : rest)


------------------------------------------------------------------------------
-- Ex 3: Finding paths.
--
-- In this exercise, you'll process mazes, described as lists like this:

maze1 :: [(String,[String])]
maze1 = [("Entry",["Pit","Corridor 1"])
        ,("Pit",[])
        ,("Corridor 1",["Entry","Dead end"])
        ,("Dead end",["Corridor 1"])
        ,("Corridor 2",["Corridor 3"])
        ,("Corridor 3",["Corridor 2"])]

-- This means that you can get from Entry to Pit or Corridor 1, and
-- from Corridor 1 you can get back to Entry or the Dead end, and so
-- forth. Here's a drawing of what maze1 looks like. Note how you
-- can't get out of the Pit, and Corridors 2 and 3 aren't connected to
-- the Entry.
--
--  Entry <--> Corridor 1 <--> Dead end
--   |
--   v         Corridor 2 <--> Corridor 3
--  Pit
--
-- Your task is to implement the function path that checks if there is
-- a path from one location to another.
--
--   path maze1 "Entry" "Pit"        ==> True
--   path maze1 "Entry" "Dead end"   ==> True
--   path maze1 "Pit"   "Entry"      ==> False
--   path maze1 "Entry" "Corridor 2" ==> False
--
-- To implement path, we'll need some helper functions. We'll work in
-- the State monad, with a state of type [String]. This tracks which
-- places we've been to.
--
-- The operation `visit maze place1` should work like this:
--   * if place1 is in the state (i.e. we've visited it before), do nothing
--   * otherwise, add place1 to the state (it has now been visited), and:
--      * for all neighbouring places of place1, run visit
--
-- PS. You might recognize this as a Depth-First Search, but if you
-- haven't heard the term, don't worry.
--
-- Examples:
--   runState (visit maze1 "Pit") []
--     ==> ((),["Pit"])
--   runState (visit maze1 "Corridor 2") []
--     ==> ((),["Corridor 3","Corridor 2"])
--   runState (visit maze1 "Entry") []
--     ==> ((),["Dead end","Corridor 1","Pit","Entry"])
--   runState (visit maze1 "Entry") ["Corridor 1"]
--     ==> ((),["Pit","Entry","Corridor 1"])


visit :: [(String,[String])] -> String -> State [String] ()
visit maze place = do
  visited <- get
  unless (place `elem` visited) $ do
    modify (place :)
    case lookup place maze of
      Just neighbours -> mapM_ (visit maze) neighbours
      Nothing -> return ()

-- Now you should be able to implement path using visit. If you run
-- visit on a place using an empty state, you'll get a state that
-- lists all the places that are reachable from the starting place.

path :: [(String,[String])] -> String -> String -> Bool
path maze start end = end `elem` evalState (visit maze start >> get) []

------------------------------------------------------------------------------
-- Ex 4: Given two lists, ks and ns, find numbers i and j from ks,
-- such that their sum i+j=n is in ns. Return all such triples
-- (i,j,n).
--
-- Use the list monad!
--
-- Examples:
--  findSum2 [1,2,3,4] [6,7]
--    ==> [(2,4,6),(3,3,6),(3,4,7),(4,2,6),(4,3,7)]
--
-- PS. The tests don't care about the order of results.

const_val_q4_True = True
const_val_q4_False = False
const_val_q4_zero = 0
const_val_q4_one = 1
const_val_q4_Nothing = Nothing
const_val_q4_empty_list = []
const_val_q4_str1 = ""

getAddResults :: Int -> Int -> Int
getAddResults x_val y_val = x_val + y_val

findAllSums :: [Int] -> [(Int, Int)]
findAllSums ks = [(i, j) | i <- ks, j <- ks]

filterValidSums :: [(Int, Int)] -> [Int] -> [(Int, Int)]
filterValidSums sums ns = filter (\(i, j) -> elem (getAddResults i j) ns) sums

findSum2 :: [Int] -> [Int] -> [(Int, Int, Int)]
findSum2 ks ns = do
  let sums = findAllSums ks
  let validSums = filterValidSums sums ns
  [(i, j, getAddResults i j) | (i, j) <- validSums]


------------------------------------------------------------------------------
-- Ex 5: compute all possible sums of elements from the given
-- list. Use the list monad.
--
-- Hint! a list literal like [True,False] or [x,0] can be useful when
-- combined with do-notation!
--
-- The order of the returned list does not matter and it may contain
-- duplicates.
--
-- Examples:
--   allSums []
--     ==> [0]
--   allSums [1]
--     ==> [1,0]
--   allSums [1,2,4]
--     ==> [7,3,5,1,6,2,4,0]

calculateSum :: [Int] -> Int
calculateSum = sum

allSums :: [Int] -> [Int]
allSums xs = [calculateSum combination | combination <- subsequences xs]

------------------------------------------------------------------------------
-- Ex 6: the standard library defines the function
--
--   foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
--
-- This function behaves like foldl, but the operation used is
-- monadic. foldM f acc xs works by running f for each element in xs,
-- giving it also the result of the previous invocation of f.
--
-- Your task is to implement the functions f1 and f2 so that the
-- functions sumBounded and sumNotTwice work.
--
-- Do not change the definitions of sumBounded and sumNotTwice. The
-- tests have their own copies of the definitions anyway.

-- sumBounded computes the sum of a list. However if the sum at any
-- point during the execution goes over the given bound, Nothing is
-- returned.
--
-- Examples:
--  sumBounded 5 [1,2,1,-2,3]
--    ==> Just 5
--  sumBounded 5 [1,2,3,1,-2]   -- 1+2+3=6 which results in Nothing
--    ==> Nothing

const_val_q6_True = True
const_val_q6_False = False
const_val_q6_zero = 0
const_val_q6_one = 1
const_val_q6_Nothing = Nothing
const_val_q6_empty_list = []
const_val_q6_str1 = ""

sumBounded :: Int -> [Int] -> Maybe Int
sumBounded k xs = foldM (f1 k) 0 xs

f1 :: Int -> Int -> Int -> Maybe Int
f1 k acc x
  | getAddResults acc x <= k = Just (getAddResults acc x)
  | otherwise = Nothing

-- sumNotTwice computes the sum of a list, but counts only the first
-- occurrence of each value.
--
-- Examples:
--  sumNotTwice [1,2,3]          ==> 6
--  sumNotTwice [1,1,2,3,2,2,3]  ==> 6
--  sumNotTwice [3,-2,3]         ==> 1
--  sumNotTwice [1,2,-2,3]       ==> 4

const_val_q7_True = True
const_val_q7_False = False
const_val_q7_zero = 0
const_val_q7_one = 1
const_val_q7_Nothing = Nothing
const_val_q7_empty_list = []
const_val_q7_str1 = ""

isVisited :: Int -> State [Int] Bool
isVisited x = do
  visited <- get
  return $ x `elem` visited

updateAndAccumulate :: Int -> State [Int] Int
updateAndAccumulate x = do
  modify (x :)
  return x

sumNotTwice :: [Int] -> Int
sumNotTwice xs = fst $ runState (foldM f2 const_val_q7_zero xs) []

f2 :: Int -> Int -> State [Int] Int
f2 acc x = do
  visited <- isVisited x
  if visited
    then return acc
    else do
      sum <- updateAndAccumulate x
      return (getAddResults acc sum)

------------------------------------------------------------------------------
-- Ex 7: here is the Result type from Set12. Implement a Monad Result
-- instance that behaves roughly like the Monad Maybe instance.
--
-- That is,
--   1. MkResult behave like Just
--   2. If part of computation produces NoResult, the whole computation
--      produces NoResult (just like Nothing)
--   3. Similarly, if we get a Failure "reason" value, the whole
--      computation produces Failure "reason"
--
-- Examples:
--   MkResult 1 >> Failure "boom" >> MkResult 2
--     ==> Failure "boom"
--   MkResult 1 >> NoResult >> Failure "not reached"
--     ==> NoResult
--   MkResult 1 >>= (\x -> MkResult (x+1))
--     ==> MkResult 2

data Result a = MkResult a | NoResult | Failure String deriving (Show, Eq)

instance Functor Result where
  fmap f (MkResult a) = MkResult (f a)
  fmap extra NoResult = NoResult
  fmap extra (Failure s) = Failure s

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  return a = MkResult a

  MkResult x >>= k = k x
  NoResult >>= extra = NoResult
  Failure x >>= extra = Failure x

------------------------------------------------------------------------------
-- Ex 8: Here is the type SL that combines the State and Logger
-- types. Implement an instance Monad SL, that behaves like the
-- combination of State and Logger. That is, state is propagated from
-- one operation to the next, and log messages are stored in the order
-- they are produced.
--
-- To simplify the type signatures, the type of the state has been set
-- to Int, instead of being a parameter like in the standard State
-- monad.
--
-- This is a tough one, probably the hardest exercise on this course!
-- You can come back to it later if you don't get it now.
--
-- You might find it easier to start with the Functor instance
--
-- Examples:
--   runSL (putSL 2 >> msgSL "hello" >> getSL) 0
--      ==> (2,2,["hello"])
--   runSL (replicateM_ 5 (modifySL (+1) >> getSL >>= \x -> msgSL ("got "++show x))) 1
--      ==> ((),6,["got 2","got 3","got 4","got 5","got 6"])

const_val_q8_True = True
const_val_q8_False = False
const_val_q8_zero = 0
const_val_q8_one = 1
const_val_q8_Nothing = Nothing
const_val_q8_empty_list = []
const_val_q8_str1 = ""

data SL a = SL (Int -> (a, Int, [String]))

runSL :: SL a -> Int -> (a, Int, [String])
runSL (SL f) state = f state

msgSL :: String -> SL ()
msgSL msg = SL (\s -> ((), s, [msg]))

getSL :: SL Int
getSL = SL (\s -> (s, s, []))

putSL :: Int -> SL ()
putSL s' = SL (\_ -> ((), s', []))

modifySL :: (Int -> Int) -> SL ()
modifySL f = SL (\s -> ((), f s, []))

instance Functor SL where
  fmap f (SL g) = SL $ \s0 ->
    let (a, s1, log) = g s0
    in (f a, s1, log)

instance Applicative SL where
  pure = return
  (<*>) = ap

passMsg :: [String] -> (a, Int, [String]) -> (a, Int, [String])
passMsg new (a, state, prev) = (a, state, new ++ prev)

instance Monad SL where
  return x = SL (\s -> (x, s, []))
  op >>= f = SL h
    where h state0 = passMsg msg $ runSL (f val1) state1
            where (val1, state1, msg) = runSL op state0

------------------------------------------------------------------------------
-- Ex 9: Implement the operation mkCounter that produces the IO operations
-- inc :: IO () and get :: IO Int. These operations should work like this:
--
--   get returns the number of times inc has been called
--
-- In other words, a simple stateful counter. Use an IORef to store the count.
--
-- Note: this is an IO operation that produces two IO operations. Thus
-- the type of mkCounter is IO (IO (), IO Int).
--
-- This exercise is tricky. Feel free to leave it until later.
--
-- An example of how mkCounter works in GHCi:
--
--  *Set11b> (inc,get) <- mkCounter
--  *Set11b> inc
--  *Set11b> inc
--  *Set11b> get
--  2
--  *Set11b> inc
--  *Set11b> inc
--  *Set11b> get
--  4

incrementCounter :: IORef Int -> IO ()
incrementCounter counterRef = modifyIORef' counterRef (+ 1)

getCounterValue :: IORef Int -> IO Int
getCounterValue counterRef = readIORef counterRef

mkCounter :: IO (IO (), IO Int)
mkCounter = do
  counterRef <- newIORef 0
  let inc = incrementCounter counterRef
      get = getCounterValue counterRef
  return (inc, get)
