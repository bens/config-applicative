{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main
  ) where

import Config.Applicative.Samples (sample0, sample1, sample2)
import Test.Utils                 (getGoldenTests)

import qualified Config.Applicative            as Cfg
import qualified Config.Applicative.Driver     as Drv
import qualified Config.Applicative.Parse      as Cfg.P
import qualified Config.Applicative.Parse.Test as Parse

import Control.Applicative    (many, some, (<|>))
import Control.Monad.Except   (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Char              (isAlphaNum)
import Data.List              (sort)
import Data.Text              (Text)
import System.FilePath        (replaceExtension, takeBaseName)
import Test.Tasty             (TestTree, testGroup)
import Test.Tasty.Golden      (findByExtension, goldenVsFile)
import Text.Printf            (printf)
import Text.Show.Pretty       (ppShow)

import qualified Config
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text.IO
import qualified Options.Applicative as Opt
import qualified Test.Tasty          as Tasty
import qualified Text.Parsec         as Parsec
import qualified Text.Parsec.Text    as Parsec
import qualified Text.PrettyPrint    as Pretty

main :: IO ()
main = do
  tests <- getTests
  Tasty.defaultMain tests

getTests :: IO TestTree
getTests = do
  parsing <- Parse.getTests
  sample0 <- getConfigGoldenTests "sample0" "SAMPLE0" sample0 "test-golden/sample0-empty"
  sample1 <- getConfigGoldenTests "sample1" "SAMPLE1" sample1 "test-golden/sample1-single-option"
  sample2 <- getConfigGoldenTests "sample2" "SAMPLE2" sample2 "test-golden/sample2-tutorial"
  pure $ testGroup "ALL" [parsing, sample0, sample1, sample2]


-- GOLDEN TESTS
--

getConfigGoldenTests :: Show a => String -> String -> Cfg.Option a -> FilePath -> IO TestTree
getConfigGoldenTests prog_name env_prefix defn test_path =
  getGoldenTests ".config" test_path $ \configPath outputPath -> do
    r <- runExceptT $ do
      xs <- liftIO $ Text.IO.readFile configPath
      (iniText, env, args) <- case Parsec.parse parseConfigFile "" xs of
        Left err -> throwError $ printf "Failed reading input file: %s\n" (show err)
        Right x  -> pure x
      ini <- case (iniText, Config.parse iniText) of
        ("",       _) -> pure (Config.Sections (Config.Position 0 0 0) [])
        (_, Left err) -> throwError $ printf "Failed to parse .cfg file: %s\n" (show err)
        (_, Right x ) -> pure x
      setup <- Drv.prepare prog_name env_prefix defn Nothing args
      case setup of
        Drv.GetConfig _ini_path args' f -> do
          cfg <- runParser prog_name ini env args' f
          pure $ printf "Parsed successfully:\n%s\n" (ppShow cfg)
        Drv.Dump _ini_path args' f -> do
          ini' <- runParser prog_name ini env args' f
          let style = Pretty.style
                { Pretty.lineLength     = 80
                , Pretty.ribbonsPerLine = 1.0
                }
              pretty = Config.pretty ini' Pretty.$+$ Pretty.text ""
          throwError (Pretty.renderStyle style pretty)
        Drv.PrintExample example -> pure example
    either (writeFile outputPath) (writeFile outputPath) r

runParser
  :: (MonadIO m, MonadError String m, Show a)
  => String
  -> Cfg.P.ConfigIn -> [(String, String)] -> [String]
  -> (Cfg.P.ConfigIn -> [(String, String)] -> Opt.ParserInfo (IO (Either [a] b)))
  -> m b
runParser prog_name ini env args' f = do
      m <- case Opt.execParserPure (Opt.prefs mempty) (f ini env) args' of
        Opt.Success x                     -> pure x
        Opt.CompletionInvoked _compl      -> throwError "OptParse completion invoked\n"
        Opt.Failure (Opt.ParserFailure g) -> throwError (printf "OptParse Failure:\n%s\n" (show h))
                                               where (h, _, _) = g prog_name
      result <- liftIO m
      case result of
        Left errs -> throwError $ unlines $ "Config.Applicative:" : ["  " ++ show e | e <- errs]
        Right x   -> pure x

parseConfigFile
  :: Parsec.Parser (Text, [(String, String)], [String])
parseConfigFile =
  (,,) <$> parseIniSection <*> parseEnvSection <*> parseCmdlineSection <* Parsec.eof
  where
    blockSep = () <$ Parsec.lookAhead (Parsec.try (Parsec.string "--- "))
    parseIniSection = do
      _ <- Parsec.string "--- INI\n"
      let parseLine = Text.pack <$> many (Parsec.satisfy (/= '\n')) <* Parsec.char '\n'
      Text.unlines <$> Parsec.manyTill parseLine blockSep
    parseEnvSection = do
      _ <- Parsec.string "--- ENV\n"
      let parseLine = do
            key <- takeWhile1 (\x -> isAlphaNum x || x == '_')
            val <- Parsec.char '=' *> takeTill (== '\n') <* Parsec.char '\n'
            pure (Text.unpack key, Text.unpack val)
      Parsec.manyTill parseLine blockSep
    parseCmdlineSection = do
      _ <- Parsec.string "--- CMDLINE\n"
      let parseLine = Parsec.sepBy (quoted <|> unquoted) spaces <* Parsec.char '\n'
            where
              spaces    = some (Parsec.char ' ')
              quoted    = Parsec.char '"' *> takeWhile1 (/= '"') <* Parsec.char '"'
              unquoted  = takeWhile1 (`notElem` ['\n', ' ', '"'])
      map Text.unpack . concat <$> many parseLine

takeTill :: (Char -> Bool) -> Parsec.Parser Text
takeTill f = Text.pack <$> many (Parsec.satisfy (not . f))

takeWhile1 :: (Char -> Bool) -> Parsec.Parser Text
takeWhile1 f = Text.pack <$> Parsec.many1 (Parsec.satisfy f)
