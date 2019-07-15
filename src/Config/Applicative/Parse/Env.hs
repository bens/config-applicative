{-# LANGUAGE TupleSections #-}

module Config.Applicative.Parse.Env
  ( findMany, findOne, findMap
  ) where

import Config.Applicative.Info        (Info(..))
import Config.Applicative.Parse.Types (ParseError(..))
import Config.Applicative.Reader      (Reader(..))
import Config.Applicative.Types       (Domain(..), Sample(..), Validation(..))

import Control.Monad    (guard)
import Data.List        (stripPrefix)
import Data.Map.Strict  (Map)
import Data.Maybe       (mapMaybe)
import Data.Traversable (for)
import Text.Printf      (printf)

import qualified Data.Map.Strict as Map

type EnvVar = String

findOne
  :: Reader a -> Info String
  -> String
  -> [(EnvVar, String)] -> Validation [ParseError] (Maybe a)
findOne rdr@(Reader psr _ppr _dom) info prefix env =
  case lookup key env of
    Nothing -> pure Nothing
    Just t  -> case psr t of
      Right x -> pure (Just x)
      Left e  -> badParse rdr key e
  where
    key = optEnvVar info prefix

findMany
  :: Reader a -> Info String
  -> String
  -> [(EnvVar, String)] -> Validation [ParseError] (Maybe [a])
findMany rdr@(Reader psr _ppr _dom) info prefix env =
  case values of
    Nothing -> pure Nothing
    Just ts -> fmap sequenceA $ for (zip keys ts) $ \(envVar,t) ->
      case psr t of
        Right x -> pure (Just x)
        Left e  -> badParse rdr envVar e
  where
    keys    = map (printf "%s_%d" (optEnvVar info prefix)) [(0::Int)..]
    keyNone = printf "%s_NONE" (optEnvVar info prefix)
    values  = case (mapMaybe' (`lookup` env) keys, lookup keyNone env) of
      (xs, Nothing) -> xs <$ guard (not (null xs))
      (xs,  Just _) -> xs <$ guard (null xs)

findMap
  :: Reader a -> Info String
  -> String
  -> [(EnvVar, String)] -> Validation [ParseError] (Maybe (Map String a))
findMap rdr@(Reader psr _ppr _dom) info prefix env =
  case values of
    Nothing -> pure Nothing
    Just xs -> fmap (Just . Map.fromList) $ for xs $ \(k, (envVar, t)) ->
      case psr t of
        Right x -> pure (k, x)
        Left e  -> badParse rdr envVar e
  where
    keys    = prefixedBy "_" (optEnvVar info prefix) $ map fst env
    keyNone = printf "%s_NONE" (optEnvVar info prefix)
    values  = case (lookupWith (`lookup` env) keys, lookup keyNone env) of
      (xs, Nothing) -> xs <$ guard (not (null xs))
      (xs,  Just _) -> xs <$ guard (null xs)

prefixedBy :: String -> String -> [EnvVar] -> [(EnvVar, String)]
prefixedBy sep prefix = mapMaybe $ \k -> (k,) <$> stripPrefix (prefix ++ sep) k

lookupWith :: (a -> Maybe b) -> [(a, k)] -> [(k, (a, b))]
lookupWith f = mapMaybe (\(a,k) -> (k,) . (a,) <$> f a)

badParse :: Reader r -> EnvVar -> String -> Validation [ParseError] a
badParse (Reader _ ppr dom) envVar e =
  Failure [EnvParseError envVar e (Sample Nothing) (Domain (map ppr <$> dom))]

mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
mapMaybe' _ [] = []
mapMaybe' f (x:xs) = case f x of
  Nothing -> []
  Just fx -> fx:mapMaybe' f xs
