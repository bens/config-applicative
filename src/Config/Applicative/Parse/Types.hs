{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config.Applicative.Parse.Types
  ( ConfigIn, ConfigOut
  , ParseError(..)
  , M(..)
  ) where

import Config.Applicative.Types (Domain(..), Key(..), Sample(..), Validation)

import Data.Functor.Compose (Compose)
import Data.Functor.Const   (Const)
import Data.Functor.Product (Product)

import qualified Config
import qualified Options.Applicative as Opt

type ConfigIn = Config.Value Config.Position
type ConfigOut = Config.Value ()

data ParseError
    -- | Error location (key,src loc), message, possibly an sample value that
    -- could have been used, and the domain of values if known.
  = ConfigParseError (Key, Config.Position) String (Sample String) Domain
    -- | Error location (env var), message, possibly an sample value that
    -- could have been used, and the domain of values if known.
  | EnvParseError String String (Sample String) Domain
    -- | A 'withIO' returned a 'Left' value.  First field is the 'withIO' label
    -- and the second is the message returned in the 'Left'.
  | CheckError String String
    deriving (Eq, Ord, Show)

newtype M a
  = M{ unM :: Compose
                (Compose Opt.Parser IO)
                (Product
                   (Const [ConfigOut])
                   (Validation [ParseError])
                ) a
     }
  deriving (Functor, Applicative)
