{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , combineConfigs
  ) where

import Config.Applicative.Info        (Info(..), optSection, optVariable)
import Config.Applicative.Option      (F(..), Option(..))
import Config.Applicative.Parse.Types
  (ConfigIn, ConfigOut, M(..), ParseError(..))
import Config.Applicative.Reader      (Reader(..), lookupReader, str)
import Config.Applicative.Types
  (Ap(..), Key(..), Metavar(..), Validation(..), runAp)

import qualified Config.Applicative.Parse.Config as Cfg
import qualified Config.Applicative.Parse.Env    as Env

import Control.Applicative   (empty, some, (<**>), (<|>))
import Control.Monad         (foldM)
import Data.Foldable         (toList)
import Data.Functor.Compose  (Compose(Compose, getCompose))
import Data.Functor.Const    (Const(Const))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Functor.Product  (Product(Pair))
import Data.List             (intersect, uncons)
import Data.Map.Strict       (Map)
import Data.Maybe            (catMaybes, fromMaybe, maybeToList)
import Data.Set              (Set)
import Data.Text             (Text)
import Text.Printf           (printf)

import qualified Config
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified Options.Applicative as Opt

import Debug.Trace

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
        commonKeys = Map.keysSet xsMap `Set.intersection` Map.keysSet ysMap
        combined = sequenceA $
          [ if xNm `Map.member` ysMap
              then let Config.Section () yNm yVal = ysMap Map.! xNm
                   in Config.Section () xNm <$> go (xNm:ks) xVal yVal
              else Right x
          | x@(Config.Section () xNm xVal) <- xs
          ] ++
          catMaybes
            [ if yNm `Map.member` xsMap then Nothing else Just (Right y)
            | y@(Config.Section () yNm yVal) <- ys
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

-- | Returns a command-line parser for `optparse-applicative` package, which if
-- it successfully parses will produce an IO action, which when run gives back
-- either the errors encountered along the way or a successful parse.
mkParser
  :: String                         -- ^ Environment variable prefix
  -> ConfigIn                       -- ^ Parsed ini-format file
  -> [(String, String)]             -- ^ Process environment
  -> Option a                       -- ^ Options
  -> Opt.Parser (IO (Either [ParseError] (a, ConfigOut)))
mkParser envVarPrefix cfg env =
  unpackM
  . runAp (mkParserOption envVarPrefix cfg env)
  . getOption
  where
    unpackM :: M a -> Opt.Parser (IO (Either [ParseError] (a, ConfigOut)))
    unpackM (M m) = fmap f <$> getCompose (getCompose m)
      where
        f (Pair (Const cfgs) (Success x)) =
          case foldM combineConfigs (Config.Sections () []) cfgs of
            Left e    -> Left [e]
            Right out -> Right (x, out)
        f (Pair (Const _) (Failure e)) = Left e

-- | This converts interprets a single carrier value of 'F' as an 'M'.  'runAp'
-- is used to combine them over an entire 'Ap F' structure.
--
-- Note that it has to recurse with 'runAp' in the 'Commands' and 'WithIO'
-- cases.
mkParserOption :: String -> ConfigIn -> [(String, String)] -> F a -> M a
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

-- FIXME: update comments
-- | Attempt to parse a value from an 'Config' file and the environment.
findValue
  :: String -> ConfigIn -> [(String, String)] -> Reader a -> Info String
  -> Validation [ParseError] (Maybe a)
findValue envVarPrefix cfg env rdr info =
  flip (<|>) <$> Cfg.findOne rdr info cfg <*> Env.findOne rdr info envVarPrefix env

-- FIXME: update comments
-- | Attempt to parse any number of values from an 'Config' file and the
-- environment.  Supports the _0, _1, etc., and _NONE environment variables.
findValues
  :: String -> ConfigIn -> [(String, String)] -> Reader a -> Info String
  -> Validation [ParseError] [a]
findValues envVarPrefix cfg env rdr info =
  f <$> Cfg.findMany rdr info cfg <*> Env.findMany rdr info envVarPrefix env
  where
    f fromCfg fromEnv = fromMaybe [] (fromEnv <|> fromCfg)

-- FIXME: update comments
-- | Attempt to parse any number of values from <variable>.<key> style variables
-- from an 'Config' file and the environment.  Each environment variable has as its
-- suffix its key in the key-value.  An empty map can be defined by the _NONE
-- environment variable.
findValuesMap
  :: String -> ConfigIn -> [(String, String)] -> Reader a -> Info String
  -> Validation [ParseError] (Map String a)
findValuesMap envVarPrefix cfg env rdr info =
  f <$> Cfg.findMap rdr info cfg <*> Env.findMap rdr info envVarPrefix env
  where
    f fromCfg fromEnv = fromMaybe mempty (fromEnv <|> fromCfg)

longO :: Opt.HasName x => Info o -> Opt.Mod x a
longO i = foldMap Opt.long (optLongs i)

shortO :: Opt.HasName x => Info o -> Opt.Mod x a
shortO i = foldMap Opt.short (optShorts i)

helpO :: Info o -> Opt.Mod x a
helpO i = foldMap Opt.help (optHelp i)

metavarO :: Opt.HasMetavar x => Info o -> Opt.Mod x a
metavarO i = let Metavar v = optMetavar i in Opt.metavar v
