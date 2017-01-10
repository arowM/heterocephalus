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
  ) where

import Text.Heterocephalus.Parse.Control (Content(..), parseLineControl)
import Text.Heterocephalus.Parse.Doc
       (Doc(..), parseDocFromControls)

docFromString :: String -> [Doc]
docFromString s =
  case parseDoc s of
    Left s' -> error s'
    Right d -> d

parseDoc :: String -> Either String [Doc]
parseDoc s = do
  controls <- parseLineControl s
  case parseDocFromControls controls of
    Left parseError -> Left $ show parseError
    Right docs -> Right docs
