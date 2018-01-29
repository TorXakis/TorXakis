module TreeMonad
(
TreeMonad,
TreeMonadT,
runTreeState,
runTreeStateT,
moveUp,
moveDown,
moveNextSibling,
getIndex,
setIndex,
indexBackward,
indexForward,
getItem,
setItem,
getTree,
getSubTree,
setSubTree,
updateItem,
updateSubItems,
updateSubTree,
)
where

import Control.Monad.Trans.State.Lazy
import qualified TreeZipper as TZ
import qualified Data.Tree as Tree
import Data.Functor.Identity as Identity

{-
State monad for the list zipper.
-}

type TreeMonad i a = TreeMonadT i Identity.Identity a
type TreeMonadT i m a = StateT (TZ.TreeZipper i) m a

runTreeState :: TreeMonad i a -> Tree.Tree i -> (a, TZ.TreeZipper i)
runTreeState m i = runState m (TZ.fromTree i)

runTreeStateT :: Monad m => TreeMonadT i m a -> Tree.Tree i -> m (a, TZ.TreeZipper i)
runTreeStateT m i = runStateT m (TZ.fromTree i)

--toTree :: TZ.TreeZipper a -> Tree.Tree a
--toTree = TZ.toTree

modifyMaybe :: Monad m => (s -> Maybe s) -> StateT s m Bool
modifyMaybe f = do
    s <- get
    case f s of
        Just s' -> put s' >> return True
        Nothing -> return False

moveUp :: Monad m => TreeMonadT i m Bool
moveUp = modifyMaybe TZ.moveUp

moveDown :: Monad m => TreeMonadT i m Bool
moveDown = modifyMaybe TZ.moveDown

moveNextSibling :: Monad m => TreeMonadT i m Bool
moveNextSibling = modifyMaybe TZ.moveNextSibling

setItem :: Monad m => i -> TreeMonadT i m ()
setItem i = modify (\pos -> TZ.setItem pos i)

getItem :: Monad m => TreeMonadT i m i
getItem = TZ.item <$> get

getSubTree :: Monad m => TreeMonadT i m (Tree.Tree i)
getSubTree = TZ.toSubTree <$> get

getTree :: Monad m => TreeMonadT i m (Tree.Tree i)
getTree = TZ.toTree <$> get

getIndex :: Monad m => TreeMonadT i m Int
getIndex = TZ.index <$> get

indexBackward :: Monad m => TreeMonadT i m Bool
indexBackward = modifyMaybe TZ.indexBackward

indexForward :: Monad m => TreeMonadT i m Bool
indexForward = modifyMaybe TZ.indexForward

setIndex :: Monad m => Int -> TreeMonadT i m Bool
setIndex i = modifyMaybe $ TZ.setIndex i

setSubTree :: Monad m => (Tree.Tree i) -> TreeMonadT i m ()
setSubTree tree = modify (TZ.setSubTree tree)

updateItem :: Monad m => (i -> i) -> TreeMonadT i m ()
updateItem f = modify $ TZ.updateItem f

updateSubItems :: Monad m => (i -> i) -> TreeMonadT i m ()
updateSubItems f = modify $ TZ.updateSubItems f

updateSubTree :: Monad m => (Tree.Tree i -> Tree.Tree i) -> TreeMonadT i m ()
updateSubTree f = modify $ TZ.updateSubTree f
