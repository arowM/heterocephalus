{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Heterocephalus.Parse.Control where

#if MIN_VERSION_base(4,9,0)
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (guard, void)
import Data.Char (isUpper)
import Data.Data (Data)
import Data.Functor (($>))
import Data.Typeable (Typeable)
import Text.Parsec
       (Parsec, (<?>), (<|>), alphaNum, between, char, choice, eof, many,
        many1, manyTill, noneOf, oneOf, option, optional, parse, sepBy,
        skipMany, spaces, string, try)
import Text.Shakespeare.Base
       (Ident(Ident), Deref, parseDeref, parseHash)

import Text.Hamlet.Parse

data Control
  = ControlForall Deref Binding
  | ControlEndForall
  | ControlIf Deref
  | ControlElse
  | ControlElseIf Deref
  | ControlEndIf
  | ControlCase Deref
  | ControlCaseOf Binding
  | ControlEndCase
  | NoControl Content
  deriving (Data, Eq, Read, Show, Typeable)

data Content = ContentRaw String
             | ContentVar Deref
    deriving (Data, Eq, Read, Show, Typeable)

type UserParser = Parsec String ()

parseLineControl :: String -> Either String [Control]
parseLineControl s =
  case parse lineControl s s of
    Left e -> Left $ show e
    Right x -> Right x

lineControl :: UserParser [Control]
lineControl = manyTill control $ try eof >> return ()

control :: UserParser Control
control = controlHash <|> controlPercent <|> controlReg
  where
    controlPercent :: UserParser Control
    controlPercent = do
      x <- parsePercent
      case x of
        Left str -> return (NoControl $ ContentRaw str)
        Right ctrl -> return ctrl

    controlHash :: UserParser Control
    controlHash = do
      x <- parseHash
      return . NoControl $
        case x of
          Left str -> ContentRaw str
          Right deref -> ContentVar deref

    controlReg :: UserParser Control
    controlReg = (NoControl . ContentRaw) <$> many (noneOf "#%")

parsePercent :: UserParser (Either String Control)
parsePercent = do
  a <- parseControl '%'
  optional eol
  return a
 where
  eol :: UserParser ()
  eol = void (char '\n') <|> void (string "\r\n")

parseControl :: Char -> UserParser (Either String Control)
parseControl c = do
  _ <- char c
  let escape = char '\\' $> Left [c]
  escape <|> (Right <$> parseControlBetweenBrackets) <|> return (Left [c])

parseControlBetweenBrackets :: UserParser Control
parseControlBetweenBrackets =
  between (char '{') (char '}') $ spaces *> parseControl' <* spaces

parseControl' :: UserParser Control
parseControl' =
  try parseForall <|> try parseEndForall <|> try parseIf <|> try parseElseIf <|>
  try parseElse <|>
  try parseEndIf <|>
  try parseCase <|>
  try parseCaseOf <|>
  try parseEndCase
  where
    parseForall :: UserParser Control
    parseForall = do
      string "forall" *> spaces
      (x, y) <- binding
      pure $ ControlForall x y

    parseEndForall :: UserParser Control
    parseEndForall = string "endforall" $> ControlEndForall

    parseIf :: UserParser Control
    parseIf = string "if" *> spaces *> fmap ControlIf parseDeref

    parseElseIf :: UserParser Control
    parseElseIf = string "elseif" *> spaces *> fmap ControlElseIf parseDeref

    parseElse :: UserParser Control
    parseElse = string "else" $> ControlElse

    parseEndIf :: UserParser Control
    parseEndIf = string "endif" $> ControlEndIf

    parseCase :: UserParser Control
    parseCase = string "case" *> spaces *> fmap ControlCase parseDeref

    parseCaseOf :: UserParser Control
    parseCaseOf = string "of" *> spaces *> fmap ControlCaseOf identPattern

    parseEndCase :: UserParser Control
    parseEndCase = string "endcase" $> ControlEndCase

    binding :: UserParser (Deref, Binding)
    binding = do
      y <- identPattern
      spaces
      _ <- string "<-"
      spaces
      x <- parseDeref
      _ <- spaceTabs
      return (x, y)

    spaceTabs :: UserParser String
    spaceTabs = many $ oneOf " \t"

    ident :: UserParser Ident
    ident = do
      i <- many1 (alphaNum <|> char '_' <|> char '\'')
      white
      return (Ident i) <?> "identifier"

    parens :: UserParser a -> UserParser a
    parens = between (char '(' >> white) (char ')' >> white)

    brackets :: UserParser a -> UserParser a
    brackets = between (char '[' >> white) (char ']' >> white)

    braces :: UserParser a -> UserParser a
    braces = between (char '{' >> white) (char '}' >> white)

    comma :: UserParser ()
    comma = char ',' >> white

    atsign :: UserParser ()
    atsign = char '@' >> white

    equals :: UserParser ()
    equals = char '=' >> white

    white :: UserParser ()
    white = skipMany $ char ' '

    wildDots :: UserParser ()
    wildDots = string ".." >> white

    isVariable :: Ident -> Bool
    isVariable (Ident (x:_)) = not (isUpper x)
    isVariable (Ident []) = error "isVariable: bad identifier"

    isConstructor :: Ident -> Bool
    isConstructor (Ident (x:_)) = isUpper x
    isConstructor (Ident []) = error "isConstructor: bad identifier"

    identPattern :: UserParser Binding
    identPattern = gcon True <|> apat
      where
        apat :: UserParser Binding
        apat = choice [varpat, gcon False, parens tuplepat, brackets listpat]

        varpat :: UserParser Binding
        varpat = do
          v <-
            try $ do
              v <- ident
              guard (isVariable v)
              return v
          option (BindVar v) $ do
            atsign
            b <- apat
            return (BindAs v b) <?> "variable"

        gcon :: Bool -> UserParser Binding
        gcon allowArgs = do
          c <-
            try $ do
              c <- dataConstr
              return c
          choice
            [ record c
            , fmap (BindConstr c) (guard allowArgs >> many apat)
            , return (BindConstr c [])
            ] <?>
            "constructor"

        dataConstr :: UserParser DataConstr
        dataConstr = do
          p <- dcPiece
          ps <- many dcPieces
          return $ toDataConstr p ps

        dcPiece :: UserParser String
        dcPiece = do
          x@(Ident y) <- ident
          guard $ isConstructor x
          return y

        dcPieces :: UserParser String
        dcPieces = do
          _ <- char '.'
          dcPiece

        toDataConstr :: String -> [String] -> DataConstr
        toDataConstr x [] = DCUnqualified $ Ident x
        toDataConstr x (y:ys) = go (x :) y ys
          where
            go :: ([String] -> [String]) -> String -> [String] -> DataConstr
            go front next [] = DCQualified (Module $ front []) (Ident next)
            go front next (rest:rests) = go (front . (next :)) rest rests

        record :: DataConstr -> UserParser Binding
        record c =
          braces $ do
            (fields, wild) <- option ([], False) go
            return (BindRecord c fields wild)
          where
            go :: UserParser ([(Ident, Binding)], Bool)
            go =
              (wildDots >> return ([], True)) <|>
              (do x <- recordField
                  (xs, wild) <- option ([], False) (comma >> go)
                  return (x : xs, wild))

        recordField :: UserParser (Ident, Binding)
        recordField = do
          field <- ident
          p <-
            option
              (BindVar field) -- support punning
              (equals >> identPattern)
          return (field, p)

        tuplepat :: UserParser Binding
        tuplepat = do
          xs <- identPattern `sepBy` comma
          return $
            case xs of
              [x] -> x
              _ -> BindTuple xs

        listpat :: UserParser Binding
        listpat = BindList <$> identPattern `sepBy` comma
