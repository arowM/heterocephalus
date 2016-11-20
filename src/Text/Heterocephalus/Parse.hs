{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Heterocephalus.Parse where

import Control.Monad
import Data.Char (isUpper)
import Data.Data
import Text.Parsec.Prim (Parsec)
import Text.ParserCombinators.Parsec hiding (Line)
import Text.Shakespeare.Base

import Text.Hamlet.Parse

data Control
  = ControlForall Deref Binding
  | ControlEndForall
  | ControlIf Deref
  | ControlElseIf Deref
  | ControlElse
  | ControlEndIf
  | NoControl Content
  deriving (Show, Eq, Read, Data, Typeable)

data Doc = DocForall Deref Binding [Doc]
         | DocCond [(Deref, [Doc])] (Maybe [Doc])
         | DocContent Content
    deriving (Show, Eq, Read, Data, Typeable)

data Content = ContentRaw String
             | ContentVar Deref
    deriving (Show, Eq, Read, Data, Typeable)

type UserParser a = Parsec String a

docFromString :: String -> [Doc]
docFromString s =
  case parseDoc s of
    Error s' -> error s'
    Ok d -> d

parseDoc :: String -> Result [Doc]
parseDoc s = do
  controls <- parseLineControl s
  return $ controlsToDocs controls

controlsToDocs :: [Control] -> [Doc]
controlsToDocs [] = []
controlsToDocs (ControlForall d b:cs) =
  let (inner, rest) = parseReps 0 cs
  in (DocForall d b $ controlsToDocs inner) : controlsToDocs rest
controlsToDocs (ControlIf d:cs) =
  let (inner, el, rest) = parseConds 0 cs
  in DocCond [(d, controlsToDocs inner)] (fmap controlsToDocs el) :
     controlsToDocs rest
controlsToDocs (NoControl c:cs) = DocContent c : controlsToDocs cs
controlsToDocs cs = error $ "Parse error: " ++ show cs

-- TODO parse elseif
parseConds :: Int -> [Control] -> ([Control], Maybe [Control], [Control])
parseConds _ [] = error "No endif found"
parseConds depth (x:xs')
  | depth < 0 =
    error "A `endif` keyword without any corresponding `if` was found."
  | depth == 0 && isEndIf x = ([], Nothing, xs')
  | depth == 0 && isElse x =
    let (ys, may, zs) = parseConds depth xs'
    in case may of
         Nothing -> ([], Just ys, zs)
         Just _ ->
           error "A `if` clause can not have more than one `else` keyword."
  | isEndIf x =
    let (ys, may, zs) = parseConds (depth - 1) xs'
    in (x : ys, may, zs)
  | isIf x =
    let (ys, may, zs) = parseConds (depth + 1) xs'
    in (x : ys, may, zs)
  | otherwise =
    let (ys, may, zs) = parseConds depth xs'
    in (x : ys, may, zs)
  where
    isIf (ControlIf _) = True
    isIf _ = False
    isEndIf ControlEndIf = True
    isEndIf _ = False
    isElse ControlElse = True
    isElse _ = False

parseReps :: Int -> [Control] -> ([Control], [Control])
parseReps _ [] = error "No endforall found"
parseReps depth (x:xs')
  | depth < 0 =
    error "A `endforall` keyword without any corresponding `for` was found."
  | depth == 0 && isEndForall x = ([], xs')
  | isEndForall x =
    let (ys, zs) = parseReps (depth - 1) xs'
    in (x : ys, zs)
  | isForall x =
    let (ys, zs) = parseReps (depth + 1) xs'
    in (x : ys, zs)
  | otherwise =
    let (ys, zs) = parseReps depth xs'
    in (x : ys, zs)
  where
    isEndForall ControlEndForall = True
    isEndForall _ = False
    isForall (ControlForall _ _) = True
    isForall _ = False

parseLineControl :: String -> Result [Control]
parseLineControl s =
  case parse lineControl s s of
    Left e -> Error $ show e
    Right x -> Ok x

lineControl :: UserParser () [Control]
lineControl = manyTill control $ try eof >> return ()

control :: UserParser () Control
control = controlHash <|> controlPercent <|> controlReg
  where
    controlPercent = do
      x <- parsePercent
      case x of
        Left str -> return (NoControl $ ContentRaw str)
        Right ctrl -> return ctrl
    controlHash = do
      x <- parseHash
      return . NoControl $
        case x of
          Left str -> ContentRaw str
          Right deref -> ContentVar deref
    controlReg = (NoControl . ContentRaw) <$> many (noneOf "#%")

parsePercent :: UserParser () (Either String Control)
parsePercent = do
  a <- parseControl '%'
  optional eol
  return a
 where
  eol = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

parseControl :: Char -> UserParser () (Either String Control)
parseControl c = do
  _ <- char c
  (char '\\' >> return (Left [c])) <|>
    (do ctrl <-
          between (char '{') (char '}') $ do
            spaces
            x <- parseControl'
            spaces
            return x
        return $ Right ctrl) <|>
    return (Left [c])


parseControl' :: UserParser () Control
parseControl' =
  try parseForall <|> try parseEndForall <|> try parseIf <|> try parseElseIf <|>
  try parseElse <|>
  try parseEndIf
  where
    parseForall = do
      _ <- try $ string "forall"
      spaces
      (x, y) <- binding
      return $ ControlForall x y
    parseEndForall = do
      _ <- try $ string "endforall"
      return $ ControlEndForall
    parseIf = do
      _ <- try $ string "if"
      spaces
      x <- parseDeref
      return $ ControlIf x
    parseElseIf = do
      _ <- try $ string "elseif"
      spaces
      x <- parseDeref
      return $ ControlElseIf x
    parseElse = do
      _ <- try $ string "else"
      return $ ControlElse
    parseEndIf = do
      _ <- try $ string "endif"
      return $ ControlEndIf
    binding = do
      y <- identPattern
      spaces
      _ <- string "<-"
      spaces
      x <- parseDeref
      _ <- spaceTabs
      return (x, y)
    spaceTabs :: Parser String
    spaceTabs = many $ oneOf " \t"
    ident :: Parser Ident
    ident =
      do i <- many1 (alphaNum <|> char '_' <|> char '\'')
         white
         return (Ident i) <?> "identifier"
    parens = between (char '(' >> white) (char ')' >> white)
    brackets = between (char '[' >> white) (char ']' >> white)
    braces = between (char '{' >> white) (char '}' >> white)
    comma = char ',' >> white
    atsign = char '@' >> white
    equals = char '=' >> white
    white = skipMany $ char ' '
    wildDots = string ".." >> white
    isVariable (Ident (x:_)) = not (isUpper x)
    isVariable (Ident []) = error "isVariable: bad identifier"
    isConstructor (Ident (x:_)) = isUpper x
    isConstructor (Ident []) = error "isConstructor: bad identifier"
    identPattern :: Parser Binding
    identPattern = gcon True <|> apat
      where
        apat = choice [varpat, gcon False, parens tuplepat, brackets listpat]
        varpat =
          do v <-
               try $ do
                 v <- ident
                 guard (isVariable v)
                 return v
             option (BindVar v) $ do
               atsign
               b <- apat
               return (BindAs v b) <?> "variable"
        gcon :: Bool -> Parser Binding
        gcon allowArgs =
          do c <-
               try $ do
                 c <- dataConstr
                 return c
             choice
               [ record c
               , fmap (BindConstr c) (guard allowArgs >> many apat)
               , return (BindConstr c [])
               ] <?> "constructor"
        dataConstr = do
          p <- dcPiece
          ps <- many dcPieces
          return $ toDataConstr p ps
        dcPiece = do
          x@(Ident y) <- ident
          guard $ isConstructor x
          return y
        dcPieces = do
          _ <- char '.'
          dcPiece
        toDataConstr x [] = DCUnqualified $ Ident x
        toDataConstr x (y:ys) = go (x :) y ys
          where
            go front next [] = DCQualified (Module $ front []) (Ident next)
            go front next (rest:rests) = go (front . (next :)) rest rests
        record c =
          braces $ do
            (fields, wild) <- option ([], False) $ go
            return (BindRecord c fields wild)
          where
            go =
              (wildDots >> return ([], True)) <|>
              (do x <- recordField
                  (xs, wild) <- option ([], False) (comma >> go)
                  return (x : xs, wild))
        recordField = do
          field <- ident
          p <-
            option
              (BindVar field) -- support punning
              (equals >> identPattern)
          return (field, p)
        tuplepat = do
          xs <- identPattern `sepBy` comma
          return $
            case xs of
              [x] -> x
              _ -> BindTuple xs
        listpat = BindList <$> identPattern `sepBy` comma
