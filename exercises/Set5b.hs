-- Exercise set 5b: playing with binary trees

module Set5b where

import Mooc.Todo

-- The next exercises use the binary tree type defined like this:

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Ex 1: implement the function valAtRoot which returns the value at
-- the root (top-most node) of the tree. The return value is Maybe a
-- because the tree might be empty (i.e. just a Empty)

const_val_q1_zero = 0
const_val_q1_one  = 1
const_val_q1_str1 = "Not an empty tree"
const_val_q1_str2 = "Not a non-empty node"

valAtRoot :: Tree a -> Maybe a
valAtRoot tree = case tree of
  Empty -> emptyTreeHandler tree
  _     -> valueExtractor tree

emptyTreeHandler :: Tree a -> Maybe a
emptyTreeHandler Empty = Nothing
emptyTreeHandler _     = error const_val_q1_str1

valueExtractor :: Tree a -> Maybe a
valueExtractor (Node value _ _) = Just value
valueExtractor _                = error const_val_q1_str2

------------------------------------------------------------------------------
-- Ex 2: compute the size of a tree, that is, the number of Node
-- constructors in it
--
-- Examples:
--   treeSize (Node 3 (Node 7 Empty Empty) Empty)  ==>  2
--   treeSize (Node 3 (Node 7 Empty Empty) (Node 1 Empty Empty))  ==>  3

const_val_q2_zero = 0
const_val_q2_one  = 1

treeSize :: Tree a -> Int
treeSize tree = nodeCounting tree

nodeCounting :: Tree a -> Int
nodeCounting Empty = const_val_q2_zero
nodeCounting (Node extra left right) = const_val_q2_one + nodeCounting left + nodeCounting right

------------------------------------------------------------------------------
-- Ex 3: get the largest value in a tree of positive Ints. The
-- largest value of an empty tree should be 0.
--
-- Examples:
--   treeMax Empty  ==>  0
--   treeMax (Node 3 (Node 5 Empty Empty) (Node 4 Empty Empty))  ==>  5

const_val_q3_zero = 0
const_val_q3_one  = 1

treeMax :: Tree Int -> Int
treeMax tree = maxValue tree

maxValue :: Tree Int -> Int
maxValue Empty = const_val_q3_zero
maxValue (Node value lValue rValue) = max value (max (maxValue lValue) (maxValue rValue))

------------------------------------------------------------------------------
-- Ex 4: implement a function that checks if all tree values satisfy a
-- condition.
--
-- Examples:
--   allValues (>0) Empty  ==>  True
--   allValues (>0) (Node 1 Empty (Node 2 Empty Empty))  ==>  True
--   allValues (>0) (Node 1 Empty (Node 0 Empty Empty))  ==>  False

allValues :: (a -> Bool) -> Tree a -> Bool
allValues condition tree = foldTree (nodeChecker condition) True tree

nodeChecker :: (a -> Bool) -> a -> Bool -> Bool -> Bool
nodeChecker condition value result_for_left result_for_right = condition value && result_for_left && result_for_right

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree extra acc Empty = acc
foldTree f acc (Node x leftValue rightValue) = f x (foldTree f acc leftValue) (foldTree f acc rightValue)

------------------------------------------------------------------------------
-- Ex 5: implement map for trees.
--
-- Examples:
--
-- mapTree (+1) Empty  ==>  Empty
-- mapTree (+2) (Node 0 (Node 1 Empty Empty) (Node 2 Empty Empty))
--   ==> (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree fVal = mapSubtree (mapValue fVal)

mapValue :: (a -> b) -> a -> b
mapValue f = f

mapSubtree :: (a -> b) -> Tree a -> Tree b
mapSubtree mapperFunc Empty = Empty
mapSubtree mapperFunc (Node value leftSection rightSection) =
    Node (mapperFunc value)           
         (mapSubtree mapperFunc leftSection) 
         (mapSubtree mapperFunc rightSection)

------------------------------------------------------------------------------
-- Ex 6: given a value and a tree, build a new tree that is the same,
-- except all nodes that contain the value have been removed. Also
-- remove the subnodes of the removed nodes.
--
-- Examples:
--
--     1          1
--    / \   ==>    \
--   2   0          0
--
--  cull 2 (Node 1 (Node 2 Empty Empty)
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty))
--
--      1           1
--     / \           \
--    2   0   ==>     0
--   / \
--  3   4
--
--  cull 2 (Node 1 (Node 2 (Node 3 Empty Empty)
--                         (Node 4 Empty Empty))
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty)
--
--    1              1
--   / \              \
--  0   3    ==>       3
--   \   \
--    2   0
--
--  cull 0 (Node 1 (Node 0 Empty
--                         (Node 2 Empty Empty))
--                 (Node 3 Empty
--                         (Node 0 Empty Empty)))
--     ==> (Node 1 Empty
--                 (Node 3 Empty Empty))

removeNodesWithValue :: Eq a => a -> Tree a -> Tree a
removeNodesWithValue _ Empty = Empty
removeNodesWithValue val (Node value left right)
  | val == value = Empty -- Remove the current node if its value matches the specified value
  | otherwise = Node value (removeNodesWithValue val left) (removeNodesWithValue val right)

removeEmptyNodes :: Tree a -> Tree a
removeEmptyNodes Empty = Empty
removeEmptyNodes (Node _ Empty Empty) = Empty -- Remove the current node if it's empty
removeEmptyNodes (Node value left right) = Node value (removeEmptyNodes left) (removeEmptyNodes right)

cull :: Eq a => a -> Tree a -> Tree a
cull val tree = removeEmptyNodes $ removeNodesWithValue val tree

------------------------------------------------------------------------------
-- Ex 7: check if a tree is ordered. A tree is ordered if:
--  * all values to the left of the root are smaller than the root value
--  * all of the values to the right of the root are larger than the root value
--  * and the left and right subtrees are ordered.
--
-- Hint: allValues will help you here!
--
-- Examples:
--         1
--        / \   is ordered:
--       0   2
--   isOrdered (Node 1 (Node 0 Empty Empty)
--                     (Node 2 Empty Empty))   ==>   True
--
--         1
--        / \   is not ordered:
--       2   3
--   isOrdered (Node 1 (Node 2 Empty Empty)
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        1     3   is not ordered:
--         \
--          0
--   isOrdered (Node 2 (Node 1 Empty
--                             (Node 0 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        0     3   is ordered:
--         \
--          1
--   isOrdered (Node 2 (Node 0 Empty
--                             (Node 1 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   True

isOrdered :: Ord a => Tree a -> Bool
isOrdered tree = checkOrder tree Nothing Nothing

checkOrder :: Ord a => Tree a -> Maybe a -> Maybe a -> Bool
checkOrder Empty extra1 extra2 = True
checkOrder (Node val leftVal rightVal) minVal maxVal =
  let isLeftOrdered = checkSubtree leftVal minVal (Just val)
      isRightOrdered = checkSubtree rightVal (Just val) maxVal
      isCurrentNodeOrdered = case (minVal, maxVal) of
        (Just minV, Just maxV) -> minV < val && val < maxV
        (Nothing, Just maxV) -> val < maxV
        (Just minV, Nothing) -> minV < val
        (Nothing, Nothing) -> True
  in isCurrentNodeOrdered && isLeftOrdered && isRightOrdered

checkSubtree :: Ord a => Tree a -> Maybe a -> Maybe a -> Bool
checkSubtree tree minVal maxVal = checkOrder tree minVal maxVal

------------------------------------------------------------------------------
-- Ex 8: a path in a tree can be represented as a list of steps that
-- go either left or right.

data Step = StepL | StepR
  deriving (Show, Eq)

-- Define a function walk that takes a tree and a list of steps, and
-- returns the value at that point. Return Nothing if you fall of the
-- tree (i.e. hit a Empty).
--
-- Examples:
--   walk [] (Node 1 (Node 2 Empty Empty) Empty)       ==>  Just 1
--   walk [StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Just 2
--   walk [StepL,StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Nothing

walk :: [Step] -> Tree a -> Maybe a
walk steps tree = foldl nextTreeGetter (Just tree) steps >>= treeValueGetter

nextTreeGetter :: Maybe (Tree a) -> Step -> Maybe (Tree a)
nextTreeGetter (Just (Node _ left right)) StepL = Just left
nextTreeGetter (Just (Node _ left right)) StepR = Just right
nextTreeGetter extra1 extra2 = Nothing

treeValueGetter :: Tree a -> Maybe a
treeValueGetter (Node val extra1 extra2) = Just val
treeValueGetter extra = Nothing

------------------------------------------------------------------------------
-- Ex 9: given a tree, a path and a value, set the value at the end of
-- the path to the given value. Since Haskell datastructures are
-- immutable, you'll need to build a new tree.
--
-- If the path falls off the tree, do nothing.
--
-- Examples:
--   set [] 1 (Node 0 Empty Empty)  ==>  (Node 1 Empty Empty)
--   set [StepL,StepL] 1 (Node 0 (Node 0 (Node 0 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--                  ==>  (Node 0 (Node 0 (Node 1 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--
--   set [StepL,StepR] 1 (Node 0 Empty Empty)  ==>  (Node 0 Empty Empty)

data Path a = EmptyPath | LeftPath (Path a) | RightPath (Path a)
  deriving (Show)

-- Helper function to update the value of a node at a given path
updateNode :: a -> Path a -> Tree a -> Tree a
updateNode newValue path tree = case (path, tree) of
    (EmptyPath, Node extra leftValue rightValue) 
      -> Node newValue leftValue rightValue
    (LeftPath restPath, Node val leftValue rightValue) 
      -> Node val (updateNode newValue restPath leftValue) rightValue
    (RightPath restPath, Node valReturned leftValue rightValue) 
      -> Node valReturned leftValue (updateNode newValue restPath rightValue)
    (extra1, extra2) -> tree

-- Convert a list of steps to a path
convertPath :: [Step] -> Path a
convertPath [] = EmptyPath
convertPath (StepL:xs) = LeftPath (convertPath xs)
convertPath (StepR:xs) = RightPath (convertPath xs)

-- Function to set the value at the end of a given path
set :: [Step] -> a -> Tree a -> Tree a
set path newVal tree = updateNode newVal (convertPath path) tree

------------------------------------------------------------------------------
-- Ex 10: given a value and a tree, return a path that goes from the
-- root to the value. If the value doesn't exist in the tree, return Nothing.
--
-- You may assume the value occurs in the tree at most once.
--
-- Examples:
--   search 1 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))  ==>  Just [StepL]
--   search 1 (Node 2 (Node 4 Empty Empty) (Node 3 Empty Empty))  ==>  Nothing
--   search 1 (Node 2 (Node 3 (Node 4 Empty Empty)
--                            (Node 1 Empty Empty))
--                    (Node 5 Empty Empty))                     ==>  Just [StepL,StepR]

search :: Eq a => a -> Tree a -> Maybe [Step]
search x tree = findPath x tree []

findPath :: Eq a => a -> Tree a -> [Step] -> Maybe [Step]
findPath extra1 Empty extra2 = Nothing
findPath x (Node val left right) path
    | x == val = Just path
    | otherwise = case (findPath x left (path ++ [StepL]), findPath x right (path ++ [StepR])) of
                    (Just leftPath, _) -> Just leftPath
                    (extra, Just rightPath) -> Just rightPath
                    extra -> Nothing