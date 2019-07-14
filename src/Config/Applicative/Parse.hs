{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

--
-- PARSING CONFIGURATION
--

-- An 'Option' is a chain of descriptions of options that we'd like to parse.
-- This is the parsing interpretation of an 'Option', what is built and returned
-- for the user to run.
--
-- The 'Compose' uses are noise and can be ignored.  The structure is:
--
-- Opt.Parser
-- -> IO
--    -> Product (pair)
--       -> Const [Config]
--       -> Validation [ParseError]
--
-- The command line parser is actually a result of parsing the 'Config' file and
-- the environment variables, so if a value is found in either of those for a
-- given variable the generated command line parser actually makes that option
-- optional, even if it's not optional to return a value, although it won't be
-- optional if no value was found in the 'Config' file or the environment.
--
-- Assuming command line parsing succeeds, an IO action will be returned which
-- allows 'WithIO' actions to be run after parsing from the sources.
--
-- A parsing failure is represented as a list of errors in a 'Validation'.
-- 'Validation's do not have a 'Monad' instance, only 'Applicative' (and
-- 'Functor') so as they are combined they collect all the errors together,
-- meaning we can return all the errors found from parsing everything provided,
-- not just failing with the first error encountered.

module Config.Applicative.Parse
  ( mkParser, ParseError(..), ConfigIn, ConfigOut
  ) where

import Config.Applicative.Info        (Info(..), optSection, optVariable)
import Config.Applicative.Option      (F(..), Option(..))
import Config.Applicative.Parse.Types
  (ConfigIn, ConfigOut, M(..), ParseError(..))
import Config.Applicative.Reader      (Reader(..), lookupReader, str)
import Config.Applicative.Types
  (Ap(..), Domain(..), Key(..), Metavar(..), Sample(..), Validation(..), runAp,
  section, variable)

import qualified Config.Applicative.Parse.Config as Cfg
import qualified Config.Applicative.Parse.Env    as Env

import Control.Applicative   (empty, some, (<**>), (<|>))
import Data.Bifunctor        (bimap)
import Data.Foldable         (find, fold, toList)
import Data.Functor.Compose  (Compose(Compose, getCompose))
import Data.Functor.Const    (Const(Const))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Functor.Product  (Product(Pair))
import Data.List             (uncons, union)
import Data.Map.Strict       (Map)
import Data.Maybe            (fromMaybe, mapMaybe, maybeToList)
import Data.Text             (Text)
import Data.Traversable      (for)
import Text.Printf           (printf)

import qualified Config
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Options.Applicative as Opt

-- | Wrap in an 'M'.
cc :: Opt.Parser (IO ([ConfigOut], Validation [ParseError] a)) -> M a
cc = M . Compose . Compose . fmap (fmap (\(inis, val) -> Pair (Const inis) val))

-- | Unwrap from an 'M'.
uu :: M a -> Opt.Parser (IO ([ConfigOut], Validation [ParseError] a))
uu =
  fmap (fmap (\(Pair (Const inis) val) -> (inis, val)))
  . getCompose . getCompose . unM

-- | Lift a command line parser for an 'a' into an 'M' 'a'.
liftPsr :: Opt.Parser a -> M a
liftPsr psr = cc ((\x -> pure (mempty, pure x)) <$> psr)

-- | Lift a list of failures into an 'M' 'a'.
liftErrs :: [ParseError] -> M a
liftErrs es = cc (pure (pure (mempty, Failure es)))

mkConfig :: (Text -> Text) -> Key -> String -> ConfigOut
mkConfig varF (Key ss v) val = foldr f (Config.Text () (Text.pack val)) ss'
  where
    f s x = Config.Sections () [Config.Section () s x]
    ss'   = map Text.pack ss ++ [varF (Text.pack v)]

combineConfigs :: [ConfigOut] -> ConfigOut
combineConfigs  [] = Config.Sections () []
combineConfigs [c] = c
combineConfigs cs  = undefined
-- combineConfigs xs = Ini.Ini
--   (foldr (\i hm -> HM.unionWith (++) (Config.iniSections i) hm) HM.empty xs)
--   (foldr (\i gs -> union (Config.iniGlobals i) gs) [] xs)

-- | If we have a 'Reader' and an 'Info' for 'a', then record the value in our
-- output 'Config'.
recording1 :: Reader a -> Info a -> M a -> M a
recording1 rdr i = fmap runIdentity . recordingN rdr i . fmap Identity

-- | If we have a 'Reader' and an 'Info' for 'a', then record a collection of
-- values in our output 'Config'.
recordingN :: Foldable f => Reader a -> Info a -> M (f a) -> M (f a)
recordingN (Reader _psr ppr _dom) i (M (Compose (Compose m))) = M (Compose (Compose (fmap f <$> m)))
  where
    f (Pair (Const inis) (Failure e))  = Pair (Const inis) (Failure e)
    f (Pair (Const inis) (Success xs)) = Pair (Const (inis ++ map g (toList xs))) (Success xs)
      where g = mkConfig id (optKey i) . ppr

-- | If we have a 'Reader' and an 'Info' for 'a', then record a 'Map' of values in
-- our output 'Config'.
recordingKV :: Reader a -> Info (String, a) -> M (Map String a) -> M (Map String a)
recordingKV (Reader _psr ppr _dom) i (M (Compose (Compose m))) = M (Compose (Compose (fmap f <$> m)))
  where
    f (Pair (Const inis) (Failure e)) = Pair (Const inis) (Failure e)
    f (Pair (Const inis) (Success x)) = Pair (Const (inis ++ map g (Map.toList x))) (Success x)
      where
        g (k, v) = mkConfig (<> "." <> Text.pack k) (optKey i) (ppr v)

-- | Returns a command-line parser for `optparse-applicative` package, which if
-- it successfully parses will produce an IO action, which when run gives back
-- either the errors encountered along the way or a successful parse.
mkParser
  :: String                         -- ^ Environment variable prefix
  -> ConfigIn                       -- ^ Parsed ini-format file
  -> [(String, String)]             -- ^ Process environment
  -> Option a                       -- ^ Options
  -> Opt.Parser (IO (Either [ParseError] (a, ConfigOut)))
mkParser envVarPrefix ini env =
  unpackM
  . runAp (mkParserOption envVarPrefix ini packedEnv)
  . getOption
  where
    packedEnv = map (bimap Text.pack Text.pack) env
    unpackM :: M a -> Opt.Parser (IO (Either [ParseError] (a, ConfigOut)))
    unpackM (M m) = fmap f <$> getCompose (getCompose m)
      where
        f (Pair (Const inis) (Success x)) = Right (x, combineConfigs inis)
        f (Pair (Const    _) (Failure e)) = Left e

-- | This converts interprets a single carrier value of 'F' as an 'M'.  'runAp'
-- is used to combine them over an entire 'Ap F' structure.
--
-- Note that it has to recurse with 'runAp' in the 'Commands' and 'WithIO'
-- cases.
mkParserOption :: String -> ConfigIn -> [(Text, Text)] -> F a -> M a
mkParserOption envVarPrefix ini env = go
  where
    go :: F a -> M a
    go = \case
      One rdr@(Reader _psr ppr _dom) i ->
        case findValue envVarPrefix ini env rdr (fmap ppr i) of
          Failure es -> liftErrs es
          Success x  -> recording1 rdr i (liftPsr (one x i rdr <|> maybe empty pure (optValue i)))
      Optional rdr@(Reader _psr ppr _dom) i ->
        case findValue envVarPrefix ini env rdr (fmap ppr i) of
          Failure es -> liftErrs es
          Success xM -> recordingN rdr i (liftPsr ((Just <$> one xM i rdr) <|> pure Nothing))
      Many rdr@(Reader _psr ppr _dom) i ->
        case findValues envVarPrefix ini env rdr (fmap ppr i) of
          Failure es -> liftErrs es
          Success xs -> recordingN rdr i (liftPsr (more i rdr <|> pure xs))
      Some rdr@(Reader _psr ppr _dom) i ->
        case findValues envVarPrefix ini env rdr (fmap ppr i) of
          Failure es -> liftErrs es
          Success [] -> recordingN rdr i (liftPsr (ne <$>  some (one Nothing i rdr)))
          Success xs -> recordingN rdr i (liftPsr (ne <$> (some (one Nothing i rdr) <|> pure xs)))
        where ne = maybe (error "unreachable") (uncurry (NE.:|)) . uncons
      Map rdr@(Reader _psr ppr _dom) i ->
        case findValuesMap envVarPrefix ini env rdr (fmap (\(k,v) -> k ++ "=" ++ ppr v) i) of
          Failure es -> liftErrs es
          Success m  -> recordingKV rdr i (liftPsr (Map.fromList <$> kv i rdr <|> pure m))
      Commands i cmds ->
        case findValue envVarPrefix ini env (lookupReader cmds) i of
          Failure es                  -> liftErrs es
          Success Nothing             -> flags cmds i
          Success (Just (_, (_, m'))) -> cc (uu (flags cmds i) <|> uu (runAp go m'))
      WithIO nm f m' ->
        let g (inis, Failure es) = pure (inis, Failure es)
            g (inis, Success x)  = f x >>= \case
              Left e  -> pure (inis, Failure [CheckError nm e])
              Right y -> pure (inis, Success y)
        in cc ((>>= g) <$> uu (runAp go m'))
    -- Build a command line parser to read a single value.
    one :: Maybe a -> Info a -> Reader a -> Opt.Parser a
    one dflt i (Reader psr ppr _dom) =
      Opt.option (Opt.eitherReader psr) $ mconcat $
        (longO i <> shortO i <> helpO i <> metavarO i)
        : [Opt.value x <> Opt.showDefaultWith ppr | x <- maybeToList dflt]
    -- Build a command line parser that reads at least one value.
    more :: Info a -> Reader a -> Opt.Parser [a]
    more i (Reader psr _ppr _dom) = some $
      Opt.option (Opt.eitherReader psr) $
        longO i <> shortO i <> helpO i <> metavarO i
    -- Build a command line parser that reads any number of values, in a
    -- "<key>=<value>" format.
    kv :: Info (String, a) -> Reader a -> Opt.Parser [(String, a)]
    kv i (Reader psr _ppr _dom) = some $
      Opt.option (Opt.eitherReader f) $
        longO i <> shortO i <> helpO i <> metavarO i
      where f x = case break (== '=') x of
              (k, '=':v) -> (,) k <$> psr v
              (_,     _) -> Left $ printf "Bad parse: %s, expected 'key=value' form" (show x)
    -- Build a command line parser to read a command switch.  It can define the
    -- subsequent options that can be parsed.
    flags :: [(String, (Maybe String, Ap F a))] -> Info String -> M a
    flags cmds i = cc $ Opt.subparser $ mconcat
      [ Opt.command nm
          (Opt.info (uu (recording1 str i (pure cmdNm) *> runAp go m') <**> Opt.helper) mempty)
      | (cmdNm, (chosenNmMay, m')) <- cmds
      , let nm = fromMaybe (printf "%s.%s.%s" (optSection i) (optVariable i) cmdNm) chosenNmMay
      ]

-- | Attempt to parse a value from an 'Config' file and the environment.
findValue
  :: String -> ConfigIn -> [(Text, Text)] -> Reader a -> Info String
  -> Validation [ParseError] (Maybe a)
findValue envVarPrefix cfg env rdr@(Reader psr _ppr _dom) i =
  case lookup (Text.pack envKey) env <|> Cfg.findOne cfg (optKey i) of
    Nothing -> pure Nothing
    Just t  -> case psr (Text.unpack t) of
      Left e  -> Failure [mkError rdr i e]
      Right x -> pure (Just x)
  where
    envKey = optEnvVar i envVarPrefix

-- | Attempt to parse any number of values from an 'Config' file and the
-- environment.  Supports the _0, _1, and _NONE environment variables.
findValues
  :: String -> ConfigIn -> [(Text, Text)] -> Reader a -> Info String
  -> Validation [ParseError] [a]
findValues envVarPrefix cfg env rdr@(Reader psr _ppr _dom) i =
  case envValues <|> Cfg.findList cfg (optKey i) of
    Nothing -> pure []
    Just ts -> for ts $ \t -> case psr (Text.unpack t) of
      Left e  -> Failure [mkError rdr i e]
      Right x -> pure x
  where
    envKeys     = map (Text.pack . printf "%s_%d" (optEnvVar i envVarPrefix)) [(0::Int)..]
    envKeyEmpty = Text.pack $ printf "%s_NONE" (optEnvVar i envVarPrefix)
    envValues   = case (takeJusts $ map (`lookup` env) envKeys, lookup envKeyEmpty env) of
      ([], Nothing) -> Nothing
      (xs, Nothing) -> Just xs
      ([], Just  _) -> Just []
      ( _, Just  _) -> Nothing

-- | Attempt to parse any number of values from <variable>.<key> style variables
-- from an 'Config' file and the environment.  Each environment variable has as its
-- suffix its key in the key-value.  An empty map can be defined by the _NONE
-- environment variable.
findValuesMap
  :: String -> ConfigIn -> [(Text, Text)] -> Reader a -> Info String
  -> Validation [ParseError] (Map String a)
findValuesMap envVarPrefix cfg env rdr@(Reader psr _ppr _dom) i =
  case envValues <|> iniValues of
    Nothing -> pure Map.empty
    Just ts -> for ts $ \t -> case psr (Text.unpack t) of
      Left e  -> Failure [mkError rdr i e]
      Right x -> pure x
  where
    section     = Text.pack (optSection i)
    keys        = undefined -- prefixedBy "." (Text.pack (optVariable i)) $ fold $ Config.keys iniSection ini
    envKeys     = prefixedBy "_" (Text.pack (optEnvVar i envVarPrefix)) $ map fst env
    envKeyEmpty = Text.pack $ printf "%s_NONE" (optEnvVar i envVarPrefix)
    -- iniValues   = Just $ lookupWith iniKeys (\v -> findHead (Config.lookupValue iniSection v ini))
    iniValues   = Cfg.findMap cfg (optKey i)
    envValues   = case (lookupWith envKeys (`lookup` env), lookup envKeyEmpty env) of
      (m, Nothing) | Map.null m -> Nothing
                   | otherwise  -> Just m
      (m, Just  _) | Map.null m -> Just m
                   | otherwise  -> Nothing
    lookupWith :: [(Text, Text)] -> (Text -> Maybe Text) -> Map String Text
    lookupWith pairs f = maybe Map.empty fold . findHead $ for pairs $ \(v, k) ->
      Map.singleton (Text.unpack k) <$> f v

-- mkError :: Reader a -> Info String -> String -> ParseError
-- mkError (Reader _psr ppr dom) i msg =
--   ParseError (optKey i) msg (optSample i) (Domain $ map ppr <$> dom)

prefixedBy :: Text -> Text -> [Text] -> [(Text, Text)]
prefixedBy sep p = mapMaybe $ \k -> (,) k <$> Text.stripPrefix (p <> sep) k

findHead :: Foldable f => f a -> Maybe a
findHead = find (const True)

takeJusts :: [Maybe a] -> [a]
takeJusts []          = []
takeJusts (Nothing:_) = []
takeJusts (Just x:xs) = x:takeJusts xs

longO :: Opt.HasName x => Info o -> Opt.Mod x a
longO i = foldMap Opt.long (optLongs i)

shortO :: Opt.HasName x => Info o -> Opt.Mod x a
shortO i = foldMap Opt.short (optShorts i)

helpO :: Info o -> Opt.Mod x a
helpO i = foldMap Opt.help (optHelp i)

metavarO :: Opt.HasMetavar x => Info o -> Opt.Mod x a
metavarO i = let Metavar v = optMetavar i in Opt.metavar v
