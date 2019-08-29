{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}

module Config.Applicative.Info
  ( Info(..), optSection, optVariable, name, long, short, envVar, help
  , metavar, value, sample, autoLong
  ) where

import Config.Applicative.Reader (Reader(..))
import Config.Applicative.Types
  (Key(..), Metavar(..), Parsed, Sample(..), section, variable)

import Data.Char   (isAlphaNum, toUpper)
import Data.List   (intercalate)
import Data.Set    (Set)
import Text.Printf (printf)

import qualified Data.Set as Set

data Info a = Info
  { optKey     :: Key
  , optLongs   :: [String]
  , optShorts  :: Set Char
  , optEnvVar  :: String -> String
  , optHelp    :: Maybe String
  , optMetavar :: Metavar
  , optValue   :: Maybe (Parsed a)
  , optSample  :: Sample a
  , optReader  :: Reader a
  } deriving Functor

optSection, optVariable :: Info a -> String
optSection  = section  . optKey
optVariable = variable . optKey

-- | Build a minimal 'Info' with section and variable names.
name :: [String] -> String -> Reader a -> Info a
name ss v rdr =
  Info (Key ss v) [] Set.empty envVarNm help' (Metavar "ARG") value' sample' rdr
  where
    envVarNm prefix =
      map (\x -> if isAlphaNum x || x == '_' then x else '_')
      $ printf "%s_%s_%s" prefix (intercalate "_" ss) (map toUpper v)
    help'   = Nothing
    value'  = Nothing
    sample' = Sample Nothing

-- | Add an additional long command line option.
long :: String -> Info a -> Info a
long nm i = i{ optLongs = optLongs i ++ [nm] }

-- | Add an additional short command line option.
short :: Char -> Info a -> Info a
short c i = i{ optShorts = Set.insert c (optShorts i) }

-- | Use a different environment variable from the default.
envVar :: String -> Info a -> Info a
envVar nm i = i{ optEnvVar = const nm }

-- | Set the help string, reproduced in command line @--help@ and when
-- generating an ini file.
help :: String -> Info a  -> Info a
help h i = i{ optHelp = Just h }

-- | Set the metavar to use in command line @--help@ and in the generated ini
-- files.
metavar :: String -> Info a -> Info a
metavar v i = i{ optMetavar = Metavar v }

-- | Set the default value of this option.  Used as an example value in
-- generated ini files.
value :: String -> Info a -> Info a
value s i = i
  { optValue  = Just x
  , optSample = optSample i <> Sample (Just x)
  }
  where
    Reader psr _ = optReader i
    Right x = psr s

-- | Set the sample value to use in generated ini files without giving the option a
-- default.
sample :: String -> Info a -> Info a
sample s i = i{ optSample = Sample (Just x) }
  where
    Reader psr _ = optReader i
    Right x = psr s

-- | Derive the default long command line option from the section and variable
-- names.
autoLong :: Info a -> Info a
autoLong i = i{ optLongs = optLongs i ++ [s ++ "." ++ v]  }
  where
    s = section  (optKey i)
    v = variable (optKey i)
