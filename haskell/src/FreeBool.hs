{-# LANGUAGE DeriveFunctor #-}
module FreeBool (FreeBool (..)) where

import BoolAlg

data FreeBool a = And (FreeBool a) (FreeBool a)
                | Or (FreeBool a) (FreeBool a)
                | Xor (FreeBool a) (FreeBool a)
                | Not (FreeBool a)
                | BTrue
                | BFalse
                | Inject a
                deriving (Show, Eq, Functor)

instance BoolAlg (FreeBool a) where
  (&&) = And
  (||) = Or
  xor = Xor
  not = Not
  true = BTrue
  false = BFalse

instance Foldable FreeBool where
  foldMap f (And lhs rhs) = foldMap f lhs <> foldMap f rhs
  foldMap f (Or lhs rhs) = foldMap f lhs <> foldMap f rhs
  foldMap f (Xor lhs rhs) = foldMap f lhs <> foldMap f rhs
  foldMap f (Not fb) = foldMap f fb
  foldMap _ BFalse = mempty
  foldMap _ BTrue = mempty
  foldMap f (Inject x) = f x

instance Traversable FreeBool where
  traverse f (And lhs rhs) = And <$> traverse f lhs <*> traverse f rhs
  traverse f (Or lhs rhs) = Or <$> traverse f lhs <*> traverse f rhs
  traverse f (Xor lhs rhs) = Xor <$> traverse f lhs <*> traverse f rhs
  traverse f (Not fb) = Not <$> traverse f fb
  traverse _ BFalse = pure BFalse
  traverse _ BTrue = pure BTrue
  traverse f (Inject x) = Inject <$> f x
