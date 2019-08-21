{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Config.Applicative.Reader
  ( Reader(..), Parsed(..)
  , str, text, auto, boolean, enum, enumCI, nonNegative, positive, redacted
  , maybeReader, eitherReader, lookupReader
  ) where

import Control.Monad (guard, when, (>=>))
import Data.Bool     (bool)
import Data.Char     (toLower)
import Data.String   (IsString(fromString))
import Data.Text     (Text)
import Text.Read     (readMaybe)

import qualified Data.Map  as Map
import qualified Data.Text as Text

data Parsed a
  = Parsed a String
    deriving Functor

data Reader a
  = Reader (String -> Either String (Parsed a)) (Maybe [(String, a)])
  deriving Functor

-- | Simply pass through the parsed string unchanged.
str :: Reader String
str = Reader f Nothing
  where f s = Right (Parsed s s)

-- | Simply pass through the parsed string unchanged, but as a 'Text' value.
text :: Reader Text
text = Reader f Nothing
  where f s = Right (Parsed (Text.pack s) s)

-- | Use the 'Read' instance to parse.
auto :: Read a => Reader a
auto = maybeReader readMaybe

-- | Accepts @"true"@ or @"false"@ without caring about case.
boolean :: Reader Bool
boolean = enumCI (bool "false" "true")

-- | Generate a __case-sensitive__ parser from a pretty-printer if your type is
-- an instance of 'Enum' and 'Bounded'.
--
-- Using this with 'Data.Int.Int64' would be a bad idea because it contains so
-- many values, instead use it on small, hand-defined, enumeration types.
enum :: (Bounded a, Enum a) => (a -> String) -> Reader a
enum f = Reader g (Just domain)
  where
    Reader g _ = maybeReader (`Map.lookup` names)
    names  = Map.fromList domain
    domain = [(f x, x) | x <- [minBound..]]

-- | Generate a __case-insensitive__ parser from a pretty-printer if your type
-- is an instance of 'Enum' and 'Bounded'.
--
-- Using this with 'Data.Int.Int64' would be a bad idea because it contains so
-- many values, instead use it on small, hand-defined, enumeration types.
enumCI :: (Bounded a, Enum a) => (a -> String) -> Reader a
enumCI f = Reader g (Just domain)
  where
    Reader g _ = maybeReader (\s -> Map.lookup (norm s) names)
    names  = Map.fromList domain
    norm   = map toLower
    domain = [(norm (f x), x) | x <- [minBound..]]

-- | Accept a 'Num' that is zero or greater.
nonNegative :: (Read a, Ord a, Num a) => Reader a
nonNegative = eitherReader (\s -> readEither s >>= check s)
  where
    check s x =
      if x >= 0 then pure x else Left (s ++ ": must be greater than or equal to 0")

-- | Accept a 'Num' that is one or greater.
positive :: (Read a, Ord a, Num a) => Reader a
positive = eitherReader (\s -> readEither s >>= check s)
  where
    check s x =
      if x > 0 then pure x else Left (s ++ ": must be greater than 0")

-- | Pass the value through unchanged, but pretty-print as a constant string.
redacted :: String -> Reader a -> Reader a
redacted displayed (Reader f _dom) = Reader g Nothing
  where
    g s = case f s of
      Left err           -> Left err
      Right (Parsed x _) -> Right (Parsed x displayed)

-- | Parse successfully with 'Just'.
maybeReader :: (String -> Maybe a) -> Reader a
maybeReader f = Reader g Nothing
  where
    g s = case f s of
      Nothing -> Left ("Bad parse: " ++ show s)
      Just x  -> Right (Parsed x s)

-- | Parse successfully with 'Right'.
eitherReader :: (String -> Either String a) -> Reader a
eitherReader f = Reader g Nothing
  where
    g s = case f s of
      Left err -> Left err
      Right x  -> Right (Parsed x s)

-- | Look up the string in an a-list.
lookupReader :: [(String, a)] -> Reader a
lookupReader cmds = Reader f (Just cmds)
  where
    f s = case s `lookup` cmds of
      Nothing  -> Left ("Bad parse: " ++ show s)
      Just cmd -> Right (Parsed cmd s)

-- HELPERS

readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
  Nothing -> Left ("Bad parse: " ++ show s)
  Just x  -> Right x
