{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}

module Config.Applicative.Types
  ( Key(..), section, variable
  , Domain(..), Metavar(..), Sample(..)
    -- * Free Applicative
 , Ap(..), liftAp, runAp, runAp_
    -- * Validation
  , Validation(..), bindV
  ) where

import Control.Applicative ((<|>))
import Data.Function       ((&))
import Data.List           (intercalate)

-- | Ini-file section and variable names.
data Key = Key [String] String
  deriving (Eq, Ord, Show)

section :: Key -> String
section (Key s _) = intercalate "." s

variable :: Key -> String
variable (Key _ v) = v

-- | The domain of values of a variable, if known.
newtype Domain = Domain (Maybe [String]) deriving (Eq, Ord, Show)

-- | The metavar string to use in help output and example files.
newtype Metavar = Metavar String deriving (Eq, Ord, Show)

-- | An optional sample value of an configuration option.
newtype Sample a = Sample (Maybe a) deriving (Eq, Ord, Show, Functor)

instance Semigroup (Sample a) where
  Sample a <> Sample b = Sample (a <|> b)

instance Monoid (Sample a) where
  mempty  = Sample Nothing
  mappend = (<>)


--
-- Free Applicative
--

data Ap f a where
  Pure :: a -> Ap f a
  Ap   :: f a -> Ap f (a -> b) -> Ap f b

instance Functor (Ap f) where
  fmap f (Pure x)   = Pure (f x)
  fmap f (Ap mx kf) = Ap mx (fmap (f .) kf)

instance Applicative (Ap f) where
  pure = Pure
  Pure f   <*> mx = fmap f mx
  Ap mx kf <*> kx = Ap mx (flip <$> kf <*> kx)

liftAp :: f a -> Ap f a
liftAp x = Ap x (Pure id)

runAp :: Applicative g => (forall x. f x -> g x) -> Ap f a -> g a
runAp _ (Pure x)   = pure x
runAp f (Ap mx kf) = (&) <$> f mx <*> runAp f kf

runAp_ :: Monoid m => (forall x. f x -> m) -> Ap f a -> m
runAp_ _ (Pure _)   = mempty
runAp_ f (Ap mx kf) = f mx <> runAp_ f kf


--
-- Validation
--

data Validation e a
  = Success a
  | Failure e

instance Functor (Validation e) where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure e) = Failure e

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Success f  <*> Success x  = Success (f x)
  Success _  <*> Failure xe = Failure xe
  Failure fe <*> Success _  = Failure fe
  Failure fe <*> Failure xe = Failure (fe <> xe)

bindV :: Validation e a -> (a -> Validation e b) -> Validation e b
bindV (Failure e) _ = Failure e
bindV (Success x) f = f x
