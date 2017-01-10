{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Heterocephalus.Parse.Doc where

#if MIN_VERSION_base(4,9,0)
#else
import Control.Applicative ((*>), pure)
#endif
import Control.Monad (void)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Text.Parsec
       (Parsec, ParseError, SourcePos, (<|>), incSourceLine, many,
        optionMaybe, parse, tokenPrim)
import Text.Shakespeare.Base (Deref)

import Text.Hamlet.Parse
import Text.Heterocephalus.Parse.Control (Content(..), Control(..))

data Doc = DocForall Deref Binding [Doc]
         | DocCond [(Deref, [Doc])] (Maybe [Doc])
         | DocContent Content
    deriving (Data, Eq, Read, Show, Typeable)

type DocParser = Parsec [Control] ()

parseDocFromControls :: [Control] -> Either ParseError [Doc]
parseDocFromControls = parse docsParser ""

docsParser :: DocParser [Doc]
docsParser = many docParser

docParser :: DocParser Doc
docParser = forallDoc <|> condDoc <|> contentDoc

contentDoc :: DocParser Doc
contentDoc = tokenPrim show position noControlToDoc
  where
    position :: SourcePos -> Control -> [Control] -> SourcePos
    position sourcePos _ _ = incSourceLine sourcePos 1

    noControlToDoc :: Control -> Maybe Doc
    noControlToDoc (NoControl content) = Just $ DocContent content
    noControlToDoc _ = Nothing

forallDoc :: DocParser Doc
forallDoc = do
  (ControlForall deref binding) <- forallControlStatement
  innerDocs <- docsParser
  void endforallControlStatement
  pure $ DocForall deref binding innerDocs

condDoc :: DocParser Doc
condDoc = do
  (ControlIf ifDeref) <- ifControlStatement
  ifInnerDocs <- docsParser
  maybeElseInnerDocs <- optionMaybe $ elseControlStatement *> docsParser
  void endifControlStatement
  pure $ DocCond [(ifDeref, ifInnerDocs)] maybeElseInnerDocs

ifControlStatement :: DocParser Control
ifControlStatement = primControlStatement $ \case
  ControlIf deref -> Just $ ControlIf deref
  _ -> Nothing

elseControlStatement :: DocParser Control
elseControlStatement = primControlStatement $ \case
  ControlElse -> Just ControlElse
  _ -> Nothing

endifControlStatement :: DocParser Control
endifControlStatement = primControlStatement $ \case
  ControlEndIf -> Just ControlEndIf
  _ -> Nothing

primControlStatement :: (Control -> Maybe Control)-> DocParser Control
primControlStatement = tokenPrim show incSourcePos

incSourcePos :: SourcePos -> a -> b -> SourcePos
incSourcePos sourcePos _ _ = incSourceLine sourcePos 1

forallControlStatement :: DocParser Control
forallControlStatement = primControlStatement $ \case
  ControlForall deref binding -> Just $ ControlForall deref binding
  _ -> Nothing

endforallControlStatement :: DocParser Control
endforallControlStatement = primControlStatement $ \case
  ControlEndForall -> Just ControlEndForall
  _ -> Nothing
