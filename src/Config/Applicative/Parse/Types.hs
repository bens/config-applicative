{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Config.Applicative.Parse.Types
  ( ConfigIn, ConfigOut
  , ParseError(..)
  , M(..)
  , P(..), composeP, natP
  ) where

import Config.Applicative.Info   (Info)
import Config.Applicative.Reader (Reader)
import Config.Applicative.Types  (Domain(..), Key(..), Sample(..), Validation)

import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Const   (Const)
import Data.Functor.Product (Product)
import Data.Map.Strict      (Map)

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
    -- | Merging two Configs failed.
  | ConfigCombineError (Maybe Key) ConfigOut ConfigOut
    deriving (Eq, Show)

newtype M a
  = M{ unM :: Compose
                (Compose Opt.Parser IO)
                (Product
                   (Const [ConfigOut])
                   (Validation [ParseError])
                ) a
     }
  deriving (Functor, Applicative)


data P m = P
  { pOne  :: forall a. Reader a -> Info String
                    -> Maybe a -> m (Maybe a)
  , pMany :: forall a. Reader a -> Info String
                    -> Maybe [a] -> m (Maybe [a])
  , pMap  :: forall a. Reader a -> Info String
                    -> Maybe (Map String a) -> m (Maybe (Map String a))
  }

composeP :: Functor n => P m -> P n -> P (Compose n m)
composeP (P af ag ah) (P bf bg bh) = P cf cg ch
  where
    cf rdr info = Compose . fmap (af rdr info) . bf rdr info
    cg rdr info = Compose . fmap (ag rdr info) . bg rdr info
    ch rdr info = Compose . fmap (ah rdr info) . bh rdr info

natP :: (forall a. n a -> m a) -> P n -> P m
natP nt (P f g h) =
  P (\r i -> nt . f r i) (\r i -> nt . g r i) (\r i -> nt . h r i)
