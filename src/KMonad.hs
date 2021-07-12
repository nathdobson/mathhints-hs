module KMonad (KMonad, kBind, kReturn, kBind2, kSequence) where

class KMonad m where
  kReturn :: Ord a => a -> m a
  kBind :: Ord a => Ord b => m a -> (a -> m b) -> m b

kBind2 :: KMonad m => Ord a => Ord b => Ord c => m a -> m b -> (a -> b -> m c) -> m c
kBind2 x y f = kBind x (kBind y . f)

kSequence :: KMonad m => Ord a => [m a] -> m [a]
kSequence [] = kReturn []
kSequence (x : xs) = kBind x (\x' -> kBind (kSequence xs) (\xs' -> kReturn $ x' : xs'))
