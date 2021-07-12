{-# LANGUAGE TupleSections #-}

module KReaderT where

import KMonad
import Variants
import Data.Tuple.Extra (first)

newtype KReaderT r m t = KReaderT {runKReader :: r -> m t}

instance KMonad m => KMonad (KReaderT r m) where
  kReturn x = KReaderT $ const $ kReturn x
  kBind x f = KReaderT (\ctx -> runKReader x ctx `kBind` (\y -> runKReader (f y) ctx))

instance KMonadVariants m => KMonadVariants (KReaderT r m) where
  kFromList xs = KReaderT (\_ -> kFromList xs)
  kUnion xs ys = KReaderT (\ctx -> kUnion (runKReader xs ctx) (runKReader ys ctx))

kWithReader :: (r' -> r) -> KReaderT r m t -> KReaderT r' m t
kWithReader f x = KReaderT (runKReader x . f)

newtype KStateT s m a = KStateT {runKState :: s -> m (a, s)}

instance (Ord s, KMonad m) => KMonad (KStateT s m) where
  kReturn x = KStateT (\s -> kReturn (x,s))
  kBind x f = KStateT (\s -> runKState x s `kBind` (\(y, s') -> runKState (f y) s'))

instance (Ord s, KMonadVariants m) => KMonadVariants (KStateT s m) where
  kFromList xs = KStateT (\s -> kFromList (map (first (,s)) xs))
  kUnion xs ys = KStateT (\s -> kUnion (runKState xs s) (runKState ys s))
