module Config.Applicative.Parse.Ini
  ( parser
  , findOne, findMany, findMap
  , combineConfigs
  , recording1, recordingN, recordingKV
  ) where

import Config.Applicative.Info        (Info(..))
import Config.Applicative.Parse.Types (M(..), P(P), ParseError(..))
import Config.Applicative.Reader      (Reader(..))
import Config.Applicative.Types
  (Domain(..), Key(..), Sample(..), Validation(..), bindV, section, variable)

import Data.Foldable    (find)
import Data.Ini         (Ini)
import Data.Map         (Map)
import Data.Text        (Text)
import Data.Traversable (for)

import qualified Data.Ini  as Ini
import qualified Data.Map  as Map
import qualified Data.Text as Text

sv :: Info a -> (Text, Text)
sv info = (Text.pack (section k), Text.pack (variable k))
  where k = optKey info

parser :: Ini -> P (Validation [ParseError])
parser ini =
  P (findOne ini) (findMany ini) (findMap ini)

findOne
  :: Ini
  -> Reader a -> Info String
  -> Maybe a -> Validation [ParseError] (Maybe a)
findOne ini rdr@(Reader psr _ppr _dom) info _ =
  case Ini.lookupArray s v ini of
    Left err  -> undefined
    Right [t] -> case psr (Text.unpack t) of
      Left err -> undefined
      Right x  -> pure (Just x)
    Right [] -> undefined
    Right ts -> undefined
  where
    (s,v) = sv info

findMany
  :: Ini
  -> Reader a -> Info String
  -> Maybe [a] -> Validation [ParseError] (Maybe [a])
findMany ini rdr@(Reader psr _ppr _dom) info _ =
  case Ini.lookupArray s v ini of
    Left err  -> undefined
    Right ts -> case traverse (psr . Text.unpack) ts of
      Left err -> undefined
      Right xs -> pure (Just xs)
  where
    (s,v) = sv info

findMap
  :: Ini
  -> Reader a -> Info String
  -> Maybe (Map String a) -> Validation [ParseError] (Maybe (Map String a))
findMap ini rdr@(Reader psr _ppr _dom) info _ =
  undefined
  where
    (s,v) = sv info
    sectionKeys = Ini.keys s ini

combineConfigs :: Ini -> Ini -> Either ParseError Ini
combineConfigs = undefined

recording1 :: Reader a -> Info a -> M a -> M a
recording1 = undefined

recordingN :: Foldable f => Reader a -> Info a -> M (f a) -> M (f a)
recordingN = undefined

recordingKV :: Reader a -> Info (String, a) -> M (Map String a) -> M (Map String a)
recordingKV = undefined
