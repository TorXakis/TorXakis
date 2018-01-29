
module TreeZipper where

import qualified Data.Tree as Tree

-----------------
-- List zipper --
-----------------
{-
Standard list zipper, in which the list is represented by a frontal (reversed) list, and a tail list.
The current element is the first element of the rear list.
-}

type ListZipper a = ([a],[a])

fromList :: [a] -> ListZipper a
fromList xs = ([], xs)

toSubList :: ListZipper a -> [a]
toSubList (_,right) = right

toList :: ListZipper a -> [a]
toList (left,right) = reverse left ++ right

elemAfter :: ListZipper a -> Maybe a
elemAfter (_, []) = Nothing
elemAfter (_, x:_) = Just x

forward :: ListZipper a -> Maybe (ListZipper a)
forward (_,[]) = Nothing
forward (xs,y:ys) = Just (y:xs, ys)

backward :: ListZipper a -> Maybe (ListZipper a)
backward ([],_) = Nothing
backward (x:xs,ys) = Just (xs, x:ys)

mapListZipper :: (a -> b) -> ListZipper a -> ListZipper b
mapListZipper f (left,right) = (f <$> left, f <$> right)

-----------------
-- Tree zipper --
-----------------
{-
Zipper representation of a rose tree. The zipper 'remembers' the current node, in order to
traverse to parents and siblings in constant time. Every node also remembers one child to
traverse to in constant time. This initially is the child at index 0, which can be incremented
or decremented in constant time. Moving upwards sets the old node as this child.
-}

data TreeZipper a = TPos
    { item :: a
    , index :: Int
    , parent :: [([TreeZipper a], a, Int ,[TreeZipper a])]
    , children :: ListZipper (TreeZipper a)
    }

-- Conversions
fromTree :: Tree.Tree a -> TreeZipper a
fromTree tree = TPos
    { item = Tree.rootLabel tree
    , index = 0
    , parent = []
    , children = fromList (fromTree <$> Tree.subForest tree)
    }

toSubTree :: TreeZipper a -> Tree.Tree a
toSubTree pos = Tree.Node
    { Tree.rootLabel = item pos
    , Tree.subForest = toTree <$> (toList . children) pos
    }

toTree :: TreeZipper a -> Tree.Tree a
toTree pos = case moveUp pos of
    Nothing -> toSubTree pos
    Just pos' -> toTree pos'

-- traversal
moveUp :: TreeZipper a -> Maybe (TreeZipper a)
moveUp pos = case parent pos of
    [] -> Nothing
    (left,it,ind,right):parents -> Just $ TPos 
        { item = it
        , index = ind
        , parent = parents
        , children = (left,pos:right)
        }

moveDown :: TreeZipper a -> Maybe (TreeZipper a)
moveDown pos = case children pos of
    (_,[]) -> Nothing
    (left,child:right) -> Just $ child{ parent = (left, item pos, index pos, right):parent pos }

moveNextSibling :: TreeZipper a -> Maybe (TreeZipper a)
moveNextSibling pos = moveUp pos >>= indexForward >>= moveDown    

-- changing index
indexBackward :: TreeZipper a -> Maybe (TreeZipper a)
indexBackward pos = do
    children' <- backward $ children pos
    return $ pos{children = children', index = index pos - 1}

indexForward :: TreeZipper a -> Maybe (TreeZipper a)
indexForward pos = do
    children' <- forward $ children pos
    return $ pos{children = children', index = index pos + 1}

setIndex :: Int -> TreeZipper a -> Maybe (TreeZipper a)
setIndex i pos
    | index pos > i = indexBackward pos >>= setIndex i
    | index pos < i = indexForward pos >>= setIndex i
    | otherwise = Just pos

-- modification
setItem :: TreeZipper a -> a -> TreeZipper a
setItem pos i = pos{item = i}

setSubTree :: Tree.Tree a -> TreeZipper a -> TreeZipper a
setSubTree tree pos =
    let subTreePos = fromTree tree
    in subTreePos{parent = parent pos}

updateItem :: (a -> a) -> TreeZipper a -> TreeZipper a
updateItem f pos = setItem pos (f $ item pos)

updateSubItems :: (a -> a) -> TreeZipper a -> TreeZipper a
updateSubItems f pos = pos {item = f $ item pos, children = mapListZipper (updateSubItems f) (children pos)}

updateSubTree :: (Tree.Tree a -> Tree.Tree a) -> TreeZipper a -> TreeZipper a
updateSubTree f pos = setSubTree (f $ toTree pos) pos

