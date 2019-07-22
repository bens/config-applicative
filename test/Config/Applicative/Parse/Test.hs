{-# LANGUAGE OverloadedStrings #-}

module Config.Applicative.Parse.Test
  ( getTests
    -- * Generators
  , genValue, genSections, genNumber, genFloating, genText, genAtom, genList
  ) where

import qualified Config.Applicative.Parse             as SUT
import qualified Config.Applicative.Parse.ConfigValue as SUT

import Control.Monad       (unless)
import Data.Bifunctor      (first)
import Data.List           (nub)
import Hedgehog            (MonadGen, property, (===))
import Test.Tasty          (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified Config
import qualified Hedgehog       as Hh
import qualified Hedgehog.Gen   as Hh.Gen
import qualified Hedgehog.Range as Hh.R

getTests :: IO TestTree
getTests =
  pure $ testGroup "Parsing"
    [ prop_combineConfigs_unit
    , prop_combineConfigs_assoc
    , prop_combineConfigs_idem
    ]

prop_combineConfigs_unit :: TestTree
prop_combineConfigs_unit =
  testProperty "combineConfigs-unit" $ property $ do
    a <- Hh.forAll genValue
    let unit = Config.Sections () []
    let x = a `SUT.combineConfigs` unit
    let y = unit `SUT.combineConfigs` a
    x === Right a
    y === Right a

prop_combineConfigs_assoc :: TestTree
prop_combineConfigs_assoc =
  testProperty "combineConfigs-associativity" $ property $ do
    a <- Hh.forAll genValue
    b <- Hh.forAll genValue
    c <- Hh.forAll genValue
    let x = (a `SUT.combineConfigs` b) >>= (`SUT.combineConfigs` c)
    let y = (b `SUT.combineConfigs` c) >>= (a `SUT.combineConfigs`)
    first (const ()) x === first (const ()) y

prop_combineConfigs_idem :: TestTree
prop_combineConfigs_idem =
  testProperty "combineConfigs-idempotence" $ property $ do
    a <- Hh.forAll genValue
    b <- Hh.forAll genValue
    let x = a `SUT.combineConfigs` b
    let l_idem = x >>= (a `SUT.combineConfigs`)
    let r_idem = x >>= (`SUT.combineConfigs` b)
    l_idem === x
    r_idem === x


--
-- GENERATORS
--

genValue :: MonadGen m => m (Config.Value ())
genValue =
  Hh.Gen.recursive Hh.Gen.choice
    [genNumber, genFloating, genText, genAtom]
    [ let m = do
            xs <- Hh.Gen.list (Hh.R.linear 0 20) $ do
              k <- Hh.Gen.text (Hh.R.linear 1 5) Hh.Gen.alphaNum
              Config.Section () k <$> genValue
            unless (length (nub (map Config.sectionName xs)) == length xs)
              Hh.Gen.discard
            pure (Config.Sections () xs)
      in Hh.Gen.subterm m id
    ]

genSections :: MonadGen m => m (Config.Value ()) -> m (Config.Value ())
genSections m = do
  xs <- Hh.Gen.list (Hh.R.linear 0 20) $
    Config.Section () <$> Hh.Gen.text (Hh.R.linear 1 5) Hh.Gen.alphaNum <*> m
  unless (length (nub (map Config.sectionName xs)) == length xs)
    Hh.Gen.discard
  pure (Config.Sections () xs)

genNumber :: MonadGen m => m (Config.Value ())
genNumber = do
  base <- Hh.Gen.element [2, 8, 10, 16]
  n    <- Hh.Gen.integral_ (Hh.R.linear (-1000) 1000)
  pure (Config.Number () base n)

genFloating :: MonadGen m => m (Config.Value ())
genFloating = do
  coef <- Hh.Gen.integral_ (Hh.R.linear (-1000) 1000)
  expn <- Hh.Gen.integral_ (Hh.R.linear (-5) 5)
  pure (Config.Floating () coef expn)

genText :: MonadGen m => m (Config.Value ())
genText = do
  t <- Hh.Gen.text (Hh.R.linear 0 80) Hh.Gen.ascii
  pure (Config.Text () t)

genAtom :: MonadGen m => m (Config.Value ())
genAtom = do
  t <- Hh.Gen.text (Hh.R.linear 0 80) Hh.Gen.alphaNum
  pure (Config.Atom () (Config.MkAtom t))

genList :: MonadGen m => m (Config.Value ()) -> m (Config.Value ())
genList m = do
  xs <- Hh.Gen.list (Hh.R.linear 0 20) m
  pure (Config.List () xs)
