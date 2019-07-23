{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  (ConfigIn, ConfigOut, M(..), P(..), ParseError(..))
import Config.Applicative.Reader      (Reader(..), lookupReader, str)
import Config.Applicative.Types
  (Ap(..), Metavar(..), Validation(..), runAp)

import qualified Config.Applicative.Parse.ConfigValue as Cfg
import qualified Config.Applicative.Parse.Env         as Env
import qualified Config.Applicative.Parse.Ini         as Ini

import Control.Applicative  (empty, some, (<**>), (<|>))
import Control.Monad        (foldM)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Const   (Const(Const))
import Data.Functor.Product (Product(Pair))
import Data.List            (uncons)
import Data.Map.Strict      (Map)
import Data.Maybe           (fromMaybe, maybeToList)
import Text.Printf          (printf)

import qualified Config
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map.Strict     as Map
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
  . runAp (mkParserOption psr)
  . getOption
  where
    psr :: P M
p    psr = undefined
    unpackM :: M a -> Opt.Parser (IO (Either [ParseError] (a, ConfigOut)))
    unpackM (M m) = fmap f <$> getCompose (getCompose m)
      where
        f (Pair (Const cfgs) (Success x)) =
          case foldM Cfg.combineConfigs (Config.Sections () []) cfgs of
            Left e    -> Left [e]
            Right out -> Right (x, out)
        f (Pair (Const _) (Failure e)) = Left e

-- | This converts interprets a single carrier value of 'F' as an 'M'.  'runAp'
-- is used to combine them over an entire 'Ap F' structure.
--
-- Note that it has to recurse with 'runAp' in the 'Commands' and 'WithIO'
-- cases.
mkParserOption :: P M -> F a -> M a
mkParserOption psr = go
  where
    go :: F a -> M a
    go = \case
      One rdr@(Reader _psr ppr _dom) info ->
        Cfg.recording1 rdr info (findValue rdr (fmap ppr info) psr)
      Optional rdr@(Reader _psr ppr _dom) info ->
        case findValue rdr (fmap ppr info) _ of
          Failure es -> liftErrs es
          Success xM -> Cfg.recordingN rdr info
            (liftPsr ((Just <$> one xM info rdr) <|> pure Nothing))
      Many rdr@(Reader _psr ppr _dom) info ->
        case findValues rdr (fmap ppr info) _ of
          Failure es -> liftErrs es
          Success xs -> Cfg.recordingN rdr info
            (liftPsr (more info rdr <|> pure xs))
      Some rdr@(Reader _psr ppr _dom) info ->
        case findValues rdr (fmap ppr info) _ of
          Failure es -> liftErrs es
          Success [] -> Cfg.recordingN rdr info
            (liftPsr (ne <$>  some (one Nothing info rdr)))
          Success xs -> Cfg.recordingN rdr info
            (liftPsr (ne <$> (some (one Nothing info rdr) <|> pure xs)))
        where ne = maybe (error "unreachable") (uncurry (NE.:|)) . uncons
      Map rdr@(Reader _psr ppr _dom) info ->
        let info' = (\(k,v) -> k ++ "=" ++ ppr v) <$> info
        in case findValuesMap rdr info' _ of
             Failure es -> liftErrs es
             Success m  -> Cfg.recordingKV rdr info
               (liftPsr (Map.fromList <$> kv info rdr <|> pure m))
      Commands info cmds ->
        case findValue (lookupReader cmds) info _ of
          Failure es                  -> liftErrs es
          Success Nothing             -> flags cmds info
          Success (Just (_, (_, m'))) -> cc (uu (flags cmds info) <|> uu (runAp go m'))
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
          (Opt.info (uu (Cfg.recording1 str i (pure cmdNm) *> runAp go m') <**> Opt.helper) mempty)
      | (cmdNm, (chosenNmMay, m')) <- cmds
      , let nm = fromMaybe (printf "%s.%s.%s" (optSection i) (optVariable i) cmdNm) chosenNmMay
      ]

-- FIXME: update comments
-- | Attempt to parse a value from an 'Config' file and the environment.
findValue
  :: Reader a -> Info String
  -> P m -> m a
findValue rdr info p = undefined
  -- flip (<|>)
  --   <$> Cfg.findOne cfg rdr info
  --   <*> Env.findOne envVarPrefix env rdr info

-- FIXME: update comments
-- | Attempt to parse any number of values from an 'Config' file and the
-- environment.  Supports the _0, _1, etc., and _NONE environment variables.
findValues
  :: ConfigIn -> String -> [(String, String)]
  -> Reader a -> Info String
  -> P m -> m [a]
findValues rdr info p = undefined
  -- (\x y -> fromMaybe [] (y <|> x))
  --   <$> Cfg.findMany cfg rdr info
  --   <*> Env.findMany envVarPrefix env rdr info

-- FIXME: update comments
-- | Attempt to parse any number of values from <variable>.<key> style variables
-- from an 'Config' file and the environment.  Each environment variable has as its
-- suffix its key in the key-value.  An empty map can be defined by the _NONE
-- environment variable.
findValuesMap
  :: String -> ConfigIn -> [(String, String)]
  -> Reader a -> Info String
  -> P m -> m (Map String a)
findValuesMap rdr info p = undefined
  -- (\x y -> fromMaybe mempty (y <|> x))
  --   <$> Cfg.findMap cfg rdr info
  --   <*> Env.findMap envVarPrefix env rdr info

longO :: Opt.HasName x => Info o -> Opt.Mod x a
longO i = foldMap Opt.long (optLongs i)

shortO :: Opt.HasName x => Info o -> Opt.Mod x a
shortO i = foldMap Opt.short (optShorts i)

helpO :: Info o -> Opt.Mod x a
helpO i = foldMap Opt.help (optHelp i)

metavarO :: Opt.HasMetavar x => Info o -> Opt.Mod x a
metavarO i = let Metavar v = optMetavar i in Opt.metavar v
