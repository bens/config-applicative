{-# LANGUAGE LambdaCase #-}

module Config.Applicative.Parse.Config
  ( findList, findOne, findMap
  ) where


import Config.Applicative.Parse.Types (ConfigIn, ParseError(..))
import Config.Applicative.Types
  (Domain(..), Key(..), Sample(..), Validation(..), bindV)

import Data.Foldable    (find)
import Data.Map         (Map)
import Data.Text        (Text)
import Data.Traversable (for)

import qualified Config
import qualified Data.Map  as Map
import qualified Data.Text as Text

findOne :: Key -> ConfigIn -> Validation [ParseError] (Maybe Text)
findOne key cfg = bindV (seek key cfg) (traverse f)
  where
    f (Config.Text _ v)                 = pure v
    f (Config.Atom _ (Config.MkAtom v)) = pure v
    f v                                 = nogood v key

findList :: Key -> ConfigIn -> Validation [ParseError] (Maybe [Text])
findList key cfg = bindV (seek key cfg) (traverse f)
  where
    f (Config.List _ xs) =
      for xs $ \case
        Config.Text _ v                 -> pure v
        Config.Atom _ (Config.MkAtom v) -> pure v
        v                               -> nogood v key
    f v = nogood v key

findMap :: Key -> ConfigIn -> Validation [ParseError] (Maybe (Map String Text))
findMap key cfg = bindV (seek key cfg) (traverse f)
  where
    f (Config.Sections _ xs) =
      fmap Map.unions $ for xs $ \case
        Config.Section _ k (Config.Text _ v) ->
          pure (Map.singleton (Text.unpack k) v)
        Config.Section _ k (Config.Atom _ (Config.MkAtom v)) ->
          pure (Map.singleton (Text.unpack k) v)
        Config.Section _ _k v -> nogood v key
    f v = nogood v key

seek :: Key -> ConfigIn -> Validation [ParseError] (Maybe ConfigIn)
seek key@(Key ss' v') cfg = go (map Text.pack (ss' ++ [v'])) cfg
  where
    go [] v = Success (Just v)
    go (s:ss) (Config.Sections _ xs) =
      case find ((== s) . Config.sectionName) xs of
        Nothing -> Success Nothing
        Just x  -> go ss (Config.sectionValue x)
    go _ v = nogood v key

nogood :: ConfigIn -> Key -> Validation [ParseError] a
nogood v key =
  Failure
    [ConfigParseError
       (key, Config.valueAnn v)
       "Bad value type"
       (Sample Nothing)
       (Domain Nothing)]
