module Main where

import Data.Monoid ((<>))
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doDocTest

doDocTest :: [String] -> IO ()
doDocTest options = doctest $ options <> ghcExtensions

ghcExtensions :: [String]
ghcExtensions =
    [ "-XTemplateHaskell"
    , "-XQuasiQuotes"
    ]
