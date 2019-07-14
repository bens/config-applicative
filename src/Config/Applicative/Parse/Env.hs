module Config.Applicative.Parse.Env
  ( findList, findOne, findMap
  ) where

import Config.Applicative.Info        (Info(..), optSection, optVariable)
import Config.Applicative.Parse.Types (ParseError(..))
import Config.Applicative.Reader      (Reader(..), lookupReader, str)
import Config.Applicative.Types
  (Domain(..), Key(..), Sample(..), Validation(..), bindV)

import Data.Map.Strict (Map)
import Data.Text       (Text)

import qualified Data.Text as Text

findOne
  :: Reader a -> Info String
  -> String -> [(Text, Text)]
  -> Validation [ParseError] (Maybe a)
findOne (Reader psr ppr dom) i prefix env =
  case lookup (Text.pack envKey) env of
    Nothing -> pure Nothing
    Just t  -> case psr (Text.unpack t) of
      Right x -> pure (Just x)
      Left e  -> Failure
        [EnvParseError envKey e (Sample Nothing) (Domain (map ppr <$> dom))]
  where
    envKey = optEnvVar i prefix

findList
  :: Reader a -> Info String
  -> String -> [(Text, Text)]
  -> Validation [ParseError] (Maybe [a])
findList (Reader psr ppr dom) i prefix env =
  undefined
  where
    -- f (Config.List _ xs) =
    --   for xs $ \case
    --     Config.Text _ v                 -> pure v
    --     Config.Atom _ (Config.MkAtom v) -> pure v
    --     v                               -> nogood v key
    -- f v = nogood v key

findMap
  :: Reader a -> Info String
  -> String -> [(Text, Text)]
  -> Validation [ParseError] (Maybe (Map String Text))
findMap (Reader psr ppr dom) i prefix env =
  undefined
  -- where
  --   f (Config.Sections _ xs) =
  --     fmap Map.unions $ for xs $ \case
  --       Config.Section _ k (Config.Text _ v) ->
  --         pure (Map.singleton (Text.unpack k) v)
  --       Config.Section _ k (Config.Atom _ (Config.MkAtom v)) ->
  --         pure (Map.singleton (Text.unpack k) v)
  --       Config.Section _ _k v -> nogood v key
  --   f v = nogood v key
