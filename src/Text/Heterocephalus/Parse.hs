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

#if MIN_VERSION_base(4,9,0)
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (guard, void)
import Data.Char (isUpper)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Text.Parsec
       (Parsec, ParseError, SourcePos, (<?>), (<|>), alphaNum, between,
        char, choice, eof, incSourceLine, many, many1, manyTill, noneOf,
        oneOf, option, optional, optionMaybe, parse, sepBy, skipMany,
        spaces, string, tokenPrim, try)
import Text.Shakespeare.Base
       (Ident(Ident), Deref, parseDeref, parseHash)

import Text.Hamlet.Parse
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
