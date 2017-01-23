{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Heterocephalus.Parse.Doc where

#if MIN_VERSION_base(4,9,0)
#else
import Control.Applicative ((*>), (<*), pure)
#endif
import Control.Monad (void)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Text.Parsec
       (Parsec, ParseError, SourcePos, (<|>), eof, incSourceLine, many,
        many1, optional, optionMaybe, parse, tokenPrim)
import Text.Shakespeare.Base (Deref)

import Text.Hamlet.Parse
import Text.Heterocephalus.Parse.Control (Content(..), Control(..))

data Doc = DocForall Deref Binding [Doc]
         | DocCond [(Deref, [Doc])] (Maybe [Doc])
         | DocCase Deref [(Binding, [Doc])]
         | DocContent Content
    deriving (Data, Eq, Read, Show, Typeable)

type DocParser = Parsec [Control] ()

parseDocFromControls :: [Control] -> Either ParseError [Doc]
parseDocFromControls = parse (docsParser <* eof) ""

docsParser :: DocParser [Doc]
docsParser = many docParser

docParser :: DocParser Doc
docParser = forallDoc <|> condDoc <|> caseDoc <|> contentDoc

forallDoc :: DocParser Doc
forallDoc = do
  ControlForall deref binding <- forallControlStatement
  innerDocs <- docsParser
  void endforallControlStatement
  pure $ DocForall deref binding innerDocs

condDoc :: DocParser Doc
condDoc = do
  ControlIf ifDeref <- ifControlStatement
  ifInnerDocs <- docsParser
  elseIfs <- condElseIfs
  maybeElseInnerDocs <- optionMaybe $ elseControlStatement *> docsParser
  void endifControlStatement
  let allConds = (ifDeref, ifInnerDocs) : elseIfs
  pure $ DocCond allConds maybeElseInnerDocs

caseDoc :: DocParser Doc
caseDoc = do
  ControlCase caseDeref <- caseControlStatement
  -- Ignore a single, optional NoControl statement (with whitespace that will be
  -- ignored).
  optional contentDoc
  caseOfs <- many1 $ do
    ControlCaseOf caseBinding <- caseOfControlStatement
    innerDocs <- docsParser
    pure (caseBinding, innerDocs)
  void endcaseControlStatement
  pure $ DocCase caseDeref caseOfs

contentDoc :: DocParser Doc
contentDoc = primControlStatement $ \case
  NoControl content -> Just $ DocContent content
  _ -> Nothing

condElseIfs :: DocParser [(Deref, [Doc])]
condElseIfs = many $ do
  ControlElseIf elseIfDeref <- elseIfControlStatement
  elseIfInnerDocs <- docsParser
  pure (elseIfDeref, elseIfInnerDocs)

ifControlStatement :: DocParser Control
ifControlStatement = primControlStatement $ \case
  ControlIf deref -> Just $ ControlIf deref
  _ -> Nothing

elseIfControlStatement :: DocParser Control
elseIfControlStatement = primControlStatement $ \case
  ControlElseIf deref -> Just $ ControlElseIf deref
  _ -> Nothing

elseControlStatement :: DocParser Control
elseControlStatement = primControlStatement $ \case
  ControlElse -> Just ControlElse
  _ -> Nothing

endifControlStatement :: DocParser Control
endifControlStatement = primControlStatement $ \case
  ControlEndIf -> Just ControlEndIf
  _ -> Nothing

caseControlStatement :: DocParser Control
caseControlStatement = primControlStatement $ \case
  ControlCase deref -> Just $ ControlCase deref
  _ -> Nothing

caseOfControlStatement :: DocParser Control
caseOfControlStatement = primControlStatement $ \case
  ControlCaseOf binding -> Just $ ControlCaseOf binding
  _ -> Nothing

endcaseControlStatement :: DocParser Control
endcaseControlStatement = primControlStatement $ \case
  ControlEndCase -> Just ControlEndCase
  _ -> Nothing

forallControlStatement :: DocParser Control
forallControlStatement = primControlStatement $ \case
  ControlForall deref binding -> Just $ ControlForall deref binding
  _ -> Nothing

endforallControlStatement :: DocParser Control
endforallControlStatement = primControlStatement $ \case
  ControlEndForall -> Just ControlEndForall
  _ -> Nothing

primControlStatement :: (Control -> Maybe x)-> DocParser x
primControlStatement = tokenPrim show incSourcePos

incSourcePos :: SourcePos -> a -> b -> SourcePos
incSourcePos sourcePos _ _ = incSourceLine sourcePos 1
