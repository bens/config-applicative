module Test.Utils
  ( getGoldenTests
  ) where

import Data.List         (sort)
import System.FilePath   (replaceExtension, takeBaseName)
import Test.Tasty        (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsFile)

getGoldenTests :: String -> FilePath -> (FilePath -> FilePath -> IO ()) -> IO TestTree
getGoldenTests extn goldenDir runit = do
  inpFiles <- sort <$> findByExtension [extn] goldenDir
  pure $ testGroup goldenDir
    [ goldenVsFile (takeBaseName inpPath) goldenPath outputPath
        (runit inpPath outputPath)
    | inpPath <- inpFiles
    , let goldenPath = replaceExtension inpPath ".golden"
          outputPath = replaceExtension inpPath ".output"
    ]
