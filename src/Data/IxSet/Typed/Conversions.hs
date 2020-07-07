{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module     : Data.IxSet.Typed.Conversions
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
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
) where

import           Control.Comonad.Zipper.Extra
import           Control.Monad
import           Control.Monad.Catch
import           Data.Hashable
import qualified Data.HashMap.Strict          as HM
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
