{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Heterocephalus.Parse
  ( module Text.Heterocephalus.Parse
  , module Text.Heterocephalus.Parse.Control
  , module Text.Heterocephalus.Parse.Doc
  , module Text.Heterocephalus.Parse.Option
  ) where

import Text.Heterocephalus.Parse.Control (Content(..), parseLineControl)
import Text.Heterocephalus.Parse.Doc
       (Doc(..), parseDocFromControls)
import Text.Heterocephalus.Parse.Option
       (ParseOptions(..), createParseOptions, defaultParseOptions)

docFromString :: ParseOptions -> String -> [Doc]
docFromString opts s =
  case parseDoc opts s of
    Left s' -> error s'
    Right d -> d

parseDoc :: ParseOptions -> String -> Either String [Doc]
parseDoc opts s = do
  controls <- parseLineControl opts s
  case parseDocFromControls controls of
    Left parseError -> Left $ show parseError
    Right docs -> Right docs
