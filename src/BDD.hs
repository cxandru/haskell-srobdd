{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}

module BDD (robdd, transform, BDDF(..), BDD, FormulaF(..), evalBdd) where
import Prelude hiding (lookup, map, init)
import Data.Map as Map (Map, (!), lookup, insert, fromAscList)
import Control.Monad.State (MonadState, get, put, modify, runState)
import Control.Monad (join)
import Data.Function (on)
import Control.Applicative (liftA2)
import Data.Bool (bool)
import Data.Fix (Fix(..), cata, ana)

data BDDF a r = LeafF Bool | BranchF a r r deriving (Show, Eq, Functor)

type BDD a = Fix (BDDF a)

pattern Leaf b = Fix (LeafF b)
pattern Branch a l r = Fix (BranchF a l r)

evalBdd :: (Ord a) => (Map a Bool) -> (BDD a) -> Bool
evalBdd σ = cata $ \case 
  LeafF b -> b
  BranchF a l r -> let right = σ ! a in
                        if right then r else l

maybeHead :: (BDDF a r) -> Maybe a
maybeHead = \case
  BranchF a _ _ -> Just a
  _ -> Nothing

data FormulaF a r = FF | TF | VF a | NegF r | BinF BinOp r r deriving (Eq, Ord, Functor)

type Formula a = Fix (FormulaF a)

pattern F = Fix FF
pattern T = Fix TF
pattern V a = Fix (VF a)
pattern Neg a = Fix (NegF a)
pattern Bin o l r = Fix (BinF o l r)

data BinOp = Disj | Conj | Imp | BiImp deriving (Enum, Eq, Ord)

interpret :: BinOp -> (Bool -> Bool -> Bool)
interpret bin = case bin of
  Disj -> (||)
  Conj -> (&&)
  Imp -> (<=)
  BiImp -> (==)

-- just for fun
vars :: Formula a -> [a]
vars = cata $ \case
  FF -> []
  TF -> []
  VF a -> return a -- VF -> return
  NegF r -> r -- NegF -> id
  BinF _ l r -> l ++ r -- BinF -> const (++)

-- | We need to store the pointers for the reduce step,
-- | Else we can't check sharing equality.
data Context a r = Context { memoize :: Map (Formula a) r
                           , pointers :: Map r (BDDF a r)
                           , bot :: r --Read
                           , top :: r --Read
                           , counter :: r
                           }
type Pointer = Int

init :: (Ord a) => Context a Pointer
init = Context (fromAscList $ [(F, 0), (T, 1)]) (fromAscList $ [(0, LeafF False), (1, LeafF True)]) 0 1 2

finalize :: (Ord a, Ord r) => Map r (BDDF a r) -> r -> BDD a
finalize map = ana ((!) map)

transform :: (Ord a) => Formula a -> BDD a
transform f = let (ptr,Context{pointers}) = runState (robdd f) init in finalize pointers ptr


createNewAndInsert :: (Ord a, MonadState (Context a Pointer) s) => (BDDF a Pointer) -> s Pointer
createNewAndInsert tree = do
  old@Context{counter,pointers} <- get
  put old{counter=counter+1, pointers = insert counter tree pointers}
  return $ counter


robdd :: (Ord a, MonadState (Context a Pointer) s) => Formula a -> s Pointer
robdd f = (get >>=) $ \Context{memoize, bot, top} ->
  case lookup f memoize of
    Just bdd -> return bdd
    Nothing -> do
      r <- case f of
             T -> return top
             F -> return bot
             V a -> createNewAndInsert $ BranchF a bot top
             Neg sf -> robdd (Bin Imp sf F) -- because of this case not a cataM
             Bin op l r -> join $ on (liftA2 $ apply op) robdd l r
      modify (\s -> s{memoize=insert f r memoize}) >> return r

toBoolMaybe :: (Eq r) => r -> r -> r -> Maybe Bool
toBoolMaybe bot top p = if p == bot then Just False
  else if p == top then Just True
  else Nothing

-- | Create a new entry if l and r differ, else reduce
smartBranch :: (Ord a, MonadState (Context a Pointer) s) => a -> Pointer -> Pointer -> s Pointer
smartBranch a l r = if l == r then return l
  else createNewAndInsert (BranchF a l r)

apply :: (Ord a, MonadState (Context a Pointer) s) => BinOp -> Pointer -> Pointer -> s Pointer
apply op l r = (get >>=) $ \Context{bot, top, pointers} ->
  case on (,) (toBoolMaybe bot top) l r of
    (Just lt, Just rr) -> return $ bool bot top (interpret op lt rr)
    _ ->
      let (ltree, rtree) = on (,) ((!) pointers) l r
          (ltop, rtop) = on (,) maybeHead ltree rtree in
        case compare ltop rtop of --structural invariant: bdd is a minheap, and we use the Maybe-adjoined ordering
          --can't both be Nothing, else we would be just be in case 0
          EQ -> let (BranchF a l1 r1, BranchF _ l2 r2) = (ltree, rtree) in
            join $ on (liftA2 $ smartBranch a) (uncurry $ apply op) (l1,l2) (r1,r2)
          x -> let (BranchF upper l1 r1, lower) = 
                     case x of
                       GT -> (ltree, r)
                       LT -> (rtree, l)
               in join $ on (liftA2 $ smartBranch upper) (uncurry $ apply op) (l1,lower) (r1,lower)
