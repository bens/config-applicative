{-# LANGUAGE LambdaCase #-}

module Config.Applicative.Parse.Config
  ( findMany, findOne, findMap
  ) where

import Config.Applicative.Info        (Info(..))
import Config.Applicative.Parse.Types (ConfigIn, ParseError(..))
import Config.Applicative.Reader      (Reader(..))
import Config.Applicative.Types
  (Domain(..), Key(..), Sample(..), Validation(..), bindV)

import Data.Foldable    (find)
import Data.Map         (Map)
import Data.Traversable (for)

import qualified Config
import qualified Data.Map  as Map
import qualified Data.Text as Text

findOne
  :: Reader a -> Info String
  -> ConfigIn -> Validation [ParseError] (Maybe a)
findOne rdr@(Reader psr _ppr _dom) info cfg =
  bindV (seek rdr key cfg) (traverse f)
  where
    key = optKey info
    f v@(Config.Text _ t)                 = p v t
    f v@(Config.Atom _ (Config.MkAtom t)) = p v t
    f v                                   = badType rdr key v
    p v = either (badParse rdr key v) pure . psr . Text.unpack

findMany
  :: Reader a -> Info String
  -> ConfigIn -> Validation [ParseError] (Maybe [a])
findMany rdr@(Reader psr _ppr _dom) info cfg =
  bindV (seek rdr key cfg) (traverse f)
  where
    key = optKey info
    f (Config.List _ xs) = for xs $ \case
      v@(Config.Text _ t)                 -> p v t
      v@(Config.Atom _ (Config.MkAtom t)) -> p v t
      v                                   -> badType rdr key v
    f v = badType rdr key v
    p v = either (badParse rdr key v) pure . psr . Text.unpack

findMap
  :: Reader a -> Info String
  -> ConfigIn -> Validation [ParseError] (Maybe (Map String a))
findMap rdr@(Reader psr _ppr _dom) info cfg =
  bindV (seek rdr key cfg) (traverse f)
  where
    key = optKey info
    f (Config.Sections _ xs) =
      fmap Map.unions $ for xs $ \case
        Config.Section _ k v -> case v of
          Config.Text _ t                 -> p v k t
          Config.Atom _ (Config.MkAtom t) -> p v k t
          _                               -> badType rdr key v
    f v = badType rdr key v
    p v k =
      either (badParse rdr key v) (pure . Map.singleton (Text.unpack k))
      . psr . Text.unpack

seek :: Reader r -> Key -> ConfigIn -> Validation [ParseError] (Maybe ConfigIn)
seek rdr key@(Key ss' v') = go (map Text.pack (ss' ++ [v']))
  where
    go [] v = Success (Just v)
    go (s:ss) (Config.Sections _ xs) =
      case find ((== s) . Config.sectionName) xs of
        Nothing -> Success Nothing
        Just x  -> go ss (Config.sectionValue x)
    go _ v = badType rdr key v

badType :: Reader r -> Key -> ConfigIn -> Validation [ParseError] a
badType (Reader _ ppr dom) key v =
  Failure [ConfigParseError (key, Config.valueAnn v) msg sample domain]
  where
    msg = "Bad value type"
    sample = Sample Nothing
    domain = Domain (map ppr <$> dom)

badParse :: Reader r -> Key -> ConfigIn -> String -> Validation [ParseError] a
badParse (Reader _ ppr dom) key v msg =
  Failure [ConfigParseError (key, Config.valueAnn v) msg sample domain]
  where
    sample = Sample Nothing
    domain = Domain (map ppr <$> dom)
