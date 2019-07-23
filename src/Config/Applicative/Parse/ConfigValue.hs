{-# LANGUAGE LambdaCase #-}

module Config.Applicative.Parse.ConfigValue
  ( parser
  , findMany, findOne, findMap
  , combineConfigs
  , recording1, recordingN, recordingKV
  ) where

import Config.Applicative.Info        (Info(..))
import Config.Applicative.Parse.Types
  (ConfigIn, ConfigOut, M(..), P(P), ParseError(..))
import Config.Applicative.Reader      (Reader(..))
import Config.Applicative.Types
  (Domain(..), Key(..), Sample(..), Validation(..), bindV)

import Data.Foldable        (find, toList)
import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Const   (Const(Const))
import Data.Functor.Product (Product(Pair))
import Data.Map             (Map)
import Data.Maybe           (catMaybes)
import Data.Traversable     (for)

import qualified Config
import qualified Data.Map  as Map
import qualified Data.Text as Text

parser :: ConfigIn -> P (Validation [ParseError])
parser cfg =
  P (findOne cfg) (findMany cfg) (findMap cfg)

findOne
  :: ConfigIn
  -> Reader a -> Info String
  -> Maybe a -> Validation [ParseError] (Maybe a)
findOne cfg rdr@(Reader psr _ppr _dom) info _ =
  bindV (seek rdr key cfg) (traverse f)
  where
    key = optKey info
    f v@(Config.Text _ t)                 = p v t
    f v@(Config.Atom _ (Config.MkAtom t)) = p v t
    f v                                   = badType rdr key v
    p v = either (badParse rdr key v) pure . psr . Text.unpack

findMany
  :: ConfigIn
  -> Reader a -> Info String
  -> Maybe [a] -> Validation [ParseError] (Maybe [a])
findMany cfg rdr@(Reader psr _ppr _dom) info _ =
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
  :: ConfigIn
  -> Reader a -> Info String
  -> Maybe (Map String a) -> Validation [ParseError] (Maybe (Map String a))
findMap cfg rdr@(Reader psr _ppr _dom) info _ =
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

combineConfigs :: ConfigOut -> ConfigOut -> Either ParseError ConfigOut
combineConfigs = go []
  where
    mkKey []     = Nothing
    mkKey (x:xs) = Just (Key (map Text.unpack (reverse xs)) (Text.unpack x))
    go ks (Config.Sections () xs) (Config.Sections () ys) =
      Config.Sections () <$> combined
      where
        xsMap = Map.fromList [(Config.sectionName x, x) | x <- xs]
        ysMap = Map.fromList [(Config.sectionName y, y) | y <- ys]
        combined = sequenceA $
          [ if xNm `Map.member` ysMap
              then let Config.Section () _ yVal = ysMap Map.! xNm
                   in Config.Section () xNm <$> go (xNm:ks) xVal yVal
              else Right x
          | x@(Config.Section () xNm xVal) <- xs
          ] ++
          catMaybes
            [ if yNm `Map.member` xsMap then Nothing else Just (Right y)
            | y@(Config.Section () yNm _) <- ys
            ]
    go _ (Config.Sections _ []) y = Right y
    go _ x (Config.Sections _ []) = Right x
    go ks x@Config.Sections{} y = Left (ConfigCombineError (mkKey ks) x y)
    go ks x y@Config.Sections{} = Left (ConfigCombineError (mkKey ks) x y)
    go ks x y = if x == y then Right x else Left (ConfigCombineError (mkKey ks) x y)

wrapConfig :: Key -> ConfigOut -> ConfigOut
wrapConfig (Key ss v) val = foldr f val ss'
  where
    f s x = Config.Sections () [Config.Section () s x]
    ss'   = map Text.pack (ss ++ [v])

-- | If we have a 'Reader' and an 'Info' for 'a', then record the value in our
-- output 'Config'.
recording1 :: Reader a -> Info a -> M a -> M a
recording1 (Reader _psr ppr _dom) i (M (Compose (Compose m))) = M (Compose (Compose (fmap f <$> m)))
  where
    f (Pair (Const cfgs) (Failure e)) = Pair (Const cfgs) (Failure e)
    f (Pair (Const cfgs) (Success x)) = Pair (Const (cfgs ++ [cfg])) (Success x)
      where
        cfg = wrapConfig (optKey i) $ Config.Text () (Text.pack (ppr x))

-- | If we have a 'Reader' and an 'Info' for 'a', then record a collection of
-- values in our output 'Config'.
recordingN :: Foldable f => Reader a -> Info a -> M (f a) -> M (f a)
recordingN (Reader _psr ppr _dom) i (M (Compose (Compose m))) = M (Compose (Compose (fmap f <$> m)))
  where
    f (Pair (Const cfgs) (Failure e))  = Pair (Const cfgs) (Failure e)
    f (Pair (Const cfgs) (Success xs)) = Pair (Const (cfgs ++ [cfg])) (Success xs)
      where
        cfg = wrapConfig (optKey i) $
          Config.List () (map (Config.Text () . Text.pack . ppr) (toList xs))

-- | If we have a 'Reader' and an 'Info' for 'a', then record a 'Map' of values in
-- our output 'Config'.
recordingKV :: Reader a -> Info (String, a) -> M (Map String a) -> M (Map String a)
recordingKV (Reader _psr ppr _dom) i (M (Compose (Compose m))) = M (Compose (Compose (fmap f <$> m)))
  where
    f (Pair (Const cfgs) (Failure e)) = Pair (Const cfgs) (Failure e)
    f (Pair (Const cfgs) (Success x)) = Pair (Const (cfgs ++ [cfg])) (Success x)
      where
        cfg = wrapConfig (optKey i) $
          Config.Sections ()
            [ Config.Section () (Text.pack k) (Config.Text () (Text.pack (ppr v)))
            | (k,v) <- Map.toList x
            ]
