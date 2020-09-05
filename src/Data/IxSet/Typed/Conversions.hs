{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module     : Data.IxSet.Typed.Conversions
   License    : MIT
   Stability  : experimental

Conversions from ixset-typed to other containers.
-}
module Data.IxSet.Typed.Conversions (
  toHashMap
, toHashMapBy
, toHashMapByM
, toZipperAsc
, toZipperDesc
, toAscCofreeList
, toAscCofreeListM
, toDescCofreeList
, toDescCofreeListM
) where

import           Control.Applicative
import           Control.Comonad.Cofree
import           Control.Comonad.Zipper.Extra
import           Control.Monad
import           Control.Monad.Catch
import           Data.Hashable
import qualified Data.HashMap.Strict          as HM
import qualified Data.List                    as L
import           Data.IxSet.Typed             as Ix
import           Data.Proxy

-- | Convert an `IxSet to a `HashMap`.
toHashMap :: (Hashable a, IsIndexOf a xs) => IxSet xs k -> HM.HashMap a [k]
toHashMap = HM.fromList . Ix.groupDescBy

-- | Convert an `IxSet` to a `HashMap` via a function on the index and associated list.
toHashMapBy :: (Hashable a, IsIndexOf a xs) => IxSet xs k -> (a -> [k] -> k') -> HM.HashMap a k'
toHashMapBy xs f = HM.fromList $ flip map (Ix.groupDescBy xs) $ \(a, ks) -> (a, f a ks)

-- | Monadic variant of `toHashMapBy`.
toHashMapByM :: (Monad m, Hashable a, IsIndexOf a xs) => IxSet xs k -> (a -> [k] -> m k') -> m (HM.HashMap a k')
toHashMapByM xs f = fmap HM.fromList $ forM (Ix.groupDescBy xs) $ \(a, ks) -> do
                       z <- f a ks
                       return (a, z)

-- | Convert an `IxSet` to a `Zipper` by descending sort on an index.
toZipperAsc :: forall proxy ix ixs a m. (IsIndexOf ix ixs, MonadThrow m) => proxy ix -> IxSet ixs a -> m (Zipper [] a)
toZipperAsc _ = zipper' . Ix.toAscList (Proxy :: Proxy ix)

-- | Convert an `IxSet` to a `Zipper` by descending sort on an index.
toZipperDesc :: forall proxy ix ixs a m. (IsIndexOf ix ixs, MonadThrow m) => proxy ix -> IxSet ixs a -> m (Zipper [] a)
toZipperDesc _ = zipper' . Ix.toDescList (Proxy :: Proxy ix)

-- | Convert an `IxSet` to a list of `Cofree` by grouping on one of its indices. The result will be sorted in ascending
-- order on the index. The elements will be sorted according to the `Ord` instance on b.
toAscCofreeList :: (Ord b, Ix.IsIndexOf ix ixs) => (ix -> c) -> (a -> c) -> (a -> b) -> Ix.IxSet ixs a -> [Cofree [] c]
toAscCofreeList f g h xs = flip map (Ix.groupAscBy xs) $ \(x, as) -> f x :< map ((:< []) . g) (L.sortOn h as)

-- | Monadic version of `toAscCofreeList`.
toAscCofreeListM :: (Ord b, Ix.IsIndexOf ix ixs, Monad m) => (ix -> m c) -> (a -> c) -> (a -> b) -> Ix.IxSet ixs a -> m [Cofree [] c]
toAscCofreeListM f g h xs = forM (Ix.groupAscBy xs) $ \(x, as) -> liftA2 (:<) (f x) (return $ map ((:< []) . g) (L.sortOn h as))

-- | Convert an `IxSet` to a list of `Cofree` by grouping on one of its indices. The result will be sorted in descending
-- order on the index. The elements will be sorted according to the `Ord` instance on b.
toDescCofreeList :: (Ord b, Ix.IsIndexOf ix ixs) => (ix -> c) -> (a -> c) -> (a -> b) -> Ix.IxSet ixs a -> [Cofree [] c]
toDescCofreeList f g h xs = flip map (Ix.groupDescBy xs) $ \(x, as) -> f x :< map ((:< []) . g) (L.sortOn h as)

-- | Monadic version of `toDescCofreeList`.
toDescCofreeListM :: (Ord b, Ix.IsIndexOf ix ixs, Monad m) => (ix -> m c) -> (a -> c) -> (a -> b) -> Ix.IxSet ixs a -> m [Cofree [] c]
toDescCofreeListM f g h xs = forM (Ix.groupDescBy xs) $ \(x, as) -> liftA2 (:<) (f x) (return $ map ((:< []) . g) (L.sortOn h as))
