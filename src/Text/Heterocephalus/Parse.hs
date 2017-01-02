{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Heterocephalus.Parse where

#if MIN_VERSION_base(4,9,0)
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (guard, void)
import Data.Char (isUpper)
import Data.Data (Data)
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

type UserParser = Parsec String ()

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

-- | Parse conditional 'Control' statements (@if@, @else@, @endif@) into a
-- three-tuple.
--
-- TODO: parse elseif
parseConds
  :: Int
  -- ^ Current recursive depth of parsing conditional statements.
  -> [Control]
  -- ^ Input 'Control' statements.  This should be every statement AFTER the
  -- initial 'ControlIf' statement.
  -> ([Control], Maybe [Control], [Control])
  -- ^ Tuple with three different sets of 'Control' statements.  The first
  -- @['Control']@ is all of the statements under the @if@ block.  The second
  -- @'Maybe' ['Control']@ is all the control statements under the @else@
  -- block.  The third @['Control']@ is the rest of the 'Control' statements
  -- after this conditional block.
parseConds _ [] = error "No endif found"
parseConds depth (x:xs)
  | depth < 0 =
    error "A `endif` keyword without any corresponding `if` was found."
  | depth == 0 && isEndIf x = ([], Nothing, xs)
  | depth == 0 && isElse x =
    let (ys, may, zs) = parseConds depth xs
    in case may of
         Nothing -> ([], Just ys, zs)
         Just _ ->
           error "A `if` clause can not have more than one `else` keyword."
  | isEndIf x =
    let (ys, may, zs) = parseConds (depth - 1) xs
    in (x : ys, may, zs)
  | isIf x =
    let (ys, may, zs) = parseConds (depth + 1) xs
    in (x : ys, may, zs)
  | otherwise =
    let (ys, may, zs) = parseConds depth xs
    in (x : ys, may, zs)
  where
    isIf :: Control -> Bool
    isIf (ControlIf _) = True
    isIf _ = False

    isEndIf :: Control -> Bool
    isEndIf ControlEndIf = True
    isEndIf _ = False

    isElse :: Control -> Bool
    isElse ControlElse = True
    isElse _ = False

parseReps :: Int -> [Control] -> ([Control], [Control])
parseReps _ [] = error "No endforall found"
parseReps depth (x:xs)
  | depth < 0 =
    error "A `endforall` keyword without any corresponding `for` was found."
  | depth == 0 && isEndForall x = ([], xs)
  | isEndForall x =
    let (ys, zs) = parseReps (depth - 1) xs
    in (x : ys, zs)
  | isForall x =
    let (ys, zs) = parseReps (depth + 1) xs
    in (x : ys, zs)
  | otherwise =
    let (ys, zs) = parseReps depth xs
    in (x : ys, zs)
  where
    isEndForall :: Control -> Bool
    isEndForall ControlEndForall = True
    isEndForall _ = False

    isForall :: Control -> Bool
    isForall (ControlForall _ _) = True
    isForall _ = False

parseLineControl :: String -> Result [Control]
parseLineControl s =
  case parse lineControl s s of
    Left e -> Error $ show e
    Right x -> Ok x

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
  (char '\\' >> return (Left [c])) <|>
    (do ctrl <-
          between (char '{') (char '}') $ do
            spaces
            x <- parseControl'
            spaces
            return x
        return $ Right ctrl) <|>
    return (Left [c])


parseControl' :: UserParser Control
parseControl' =
  try parseForall <|> try parseEndForall <|> try parseIf <|> try parseElse <|>
  try parseEndIf
  where
    parseForall :: UserParser Control
    parseForall = do
      _ <- try $ string "forall"
      spaces
      (x, y) <- binding
      return $ ControlForall x y

    parseEndForall :: UserParser Control
    parseEndForall = do
      _ <- try $ string "endforall"
      return $ ControlEndForall

    parseIf :: UserParser Control
    parseIf = do
      _ <- try $ string "if"
      spaces
      x <- parseDeref
      return $ ControlIf x

    parseElse :: UserParser Control
    parseElse = do
      _ <- try $ string "else"
      return $ ControlElse

    parseEndIf :: UserParser Control
    parseEndIf = do
      _ <- try $ string "endif"
      return $ ControlEndIf

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
