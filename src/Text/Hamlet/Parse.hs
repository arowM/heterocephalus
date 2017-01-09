{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Hamlet.Parse
    ( Binding (..)
    , specialOrIdent
    , DataConstr (..)
    , Module (..)
    )
    where

import Text.Shakespeare.Base
import Control.Applicative (Applicative (..))
import Control.Monad
import Data.Data

data Binding = BindVar Ident
             | BindAs Ident Binding
             | BindConstr DataConstr [Binding]
             | BindTuple [Binding]
             | BindList [Binding]
             | BindRecord DataConstr [(Ident, Binding)] Bool
    deriving (Eq, Show, Read, Data, Typeable)

data DataConstr = DCQualified Module Ident
                | DCUnqualified Ident
    deriving (Eq, Show, Read, Data, Typeable)

newtype Module = Module [String]
    deriving (Eq, Show, Read, Data, Typeable)

-- | This funny hack is to allow us to refer to the 'or' function without
-- requiring the user to have it in scope. See how this function is used in
-- Text.Hamlet.
specialOrIdent :: Ident
specialOrIdent = Ident "__or__hamlet__special"
