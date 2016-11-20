{-# LANGUAGE TemplateHaskell #-}

module Text.Heterocephalus
  (
  -- * Core functions
    compile
  , compileFile

  -- * low-level
  , compileFromString
  ) where

import Data.Char (isDigit)
import qualified Data.Foldable as F
import Data.List (intercalate)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (Module)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Internal (preEscapedText)
import Text.Hamlet
import Text.Hamlet.Parse
import Text.Shakespeare.Base

import Text.Heterocephalus.Parse (Doc(..), Content(..), parseDoc)

{- $setup
  >>> :set -XTemplateHaskell -XQuasiQuotes
  >>> import Text.Blaze.Html.Renderer.String
-}

{-| Heterocephalus quasi-quoter.

  >>> renderHtml (let as = ["a", "b"] in [compile|sample %{ forall a <- as }key: #{a}, %{ endforall }|] "")
  "sample key: a, key: b, "

  >>> renderHtml (let num=2 in [compile|#{num} is %{ if even num }even number.%{ else }odd number.%{ endif }|] "")
  "2 is even number."
 -}
compile :: QuasiQuoter
compile =
  QuasiQuoter
  { quoteExp = compileFromString
  , quotePat = error "not used"
  , quoteType = error "not used"
  , quoteDec = error "not used"
  }

{-| Compile a template file.
-}
compileFile :: FilePath -> Q Exp
compileFile fp = do
  qAddDependentFile fp
  contents <- fmap TL.unpack $ qRunIO $ readUtf8File fp
  compileFromString contents

compileFromString :: String -> Q Exp
compileFromString s =
  docsToExp [] $ docFromString s

docFromString :: String -> [Doc]
docFromString s =
  case parseDoc s of
    Error s' -> error s'
    Ok d -> d

-- ==============================================
--  Helper functions
-- ==============================================

docsToExp :: Scope -> [Doc] -> Q Exp
docsToExp scope docs = do
  exps <- mapM (docToExp scope) docs
  case exps of
    [] -> [|return ()|]
    [x] -> return x
    _ -> return $ DoE $ map NoBindS exps

-- TODO What's scope?
docToExp :: Scope -> Doc -> Q Exp
docToExp scope (DocForall list idents inside) = do
  let list' = derefToExp scope list
  (pat, extraScope) <- bindingPattern idents
  let scope' = extraScope ++ scope
  mh <- [|F.mapM_|]
  inside' <- docsToExp scope' inside
  let lam = LamE [pat] inside'
  return $ mh `AppE` lam `AppE` list'
docToExp scope (DocCond conds final) = do
  conds' <- mapM go conds
  final' <-
    case final of
      Nothing -> [|Nothing|]
      Just f -> do
        f' <- docsToExp scope f
        j <- [|Just|]
        return $ j `AppE` f'
  ch <- [|condH|]
  return $ ch `AppE` ListE conds' `AppE` final'
  where
    go :: (Deref, [Doc]) -> Q Exp
    go (d, docs) = do
      let d' = derefToExp ((specialOrIdent, VarE 'or) : scope) d
      docs' <- docsToExp scope docs
      return $ TupE [d', docs']
docToExp v (DocContent c) = contentToExp v c

contentToExp :: Scope -> Content -> Q Exp
contentToExp _ (ContentRaw s) = do
  os <- [|preEscapedText . pack|]
  let s' = LitE $ StringL s
  i <- [|id|]
  return $ i `AppE` (os `AppE` s')
contentToExp scope (ContentVar d) = do
  str <- [|toHtml|]
  i <- [|id|]
  return $ i `AppE` (str `AppE` derefToExp scope d)

-- ==============================================
-- Codes from Text.Hamlet that is not exposed
-- ==============================================

unIdent :: Ident -> String
unIdent (Ident s) = s

bindingPattern :: Binding -> Q (Pat, [(Ident, Exp)])
bindingPattern (BindAs i@(Ident s) b) = do
  name <- newName s
  (pattern, scope) <- bindingPattern b
  return (AsP name pattern, (i, VarE name) : scope)
bindingPattern (BindVar i@(Ident s))
  | s == "_" = return (WildP, [])
  | all isDigit s = do return (LitP $ IntegerL $ read s, [])
  | otherwise = do
    name <- newName s
    return (VarP name, [(i, VarE name)])
bindingPattern (BindTuple is) = do
  (patterns, scopes) <- fmap unzip $ mapM bindingPattern is
  return (TupP patterns, concat scopes)
bindingPattern (BindList is) = do
  (patterns, scopes) <- fmap unzip $ mapM bindingPattern is
  return (ListP patterns, concat scopes)
bindingPattern (BindConstr con is) = do
  (patterns, scopes) <- fmap unzip $ mapM bindingPattern is
  return (ConP (mkConName con) patterns, concat scopes)
bindingPattern (BindRecord con fields wild) = do
  let f (Ident field, b) = do
        (p, s) <- bindingPattern b
        return ((mkName field, p), s)
  (patterns, scopes) <- fmap unzip $ mapM f fields
  (patterns1, scopes1) <-
    if wild
      then bindWildFields con $ map fst fields
      else return ([], [])
  return
    (RecP (mkConName con) (patterns ++ patterns1), concat scopes ++ scopes1)

mkConName :: DataConstr -> Name
mkConName = mkName . conToStr

conToStr :: DataConstr -> String
conToStr (DCUnqualified (Ident x)) = x
conToStr (DCQualified (Module xs) (Ident x)) = intercalate "." $ xs ++ [x]

-- Wildcards bind all of the unbound fields to variables whose name
-- matches the field name.
--
-- For example: data R = C { f1, f2 :: Int }
-- C {..}           is equivalent to   C {f1=f1, f2=f2}
-- C {f1 = a, ..}   is equivalent to   C {f1=a,  f2=f2}
-- C {f2 = a, ..}   is equivalent to   C {f1=f1, f2=a}
bindWildFields :: DataConstr -> [Ident] -> Q ([(Name, Pat)], [(Ident, Exp)])
bindWildFields conName fields = do
  fieldNames <- recordToFieldNames conName
  let available n = nameBase n `notElem` map unIdent fields
  let remainingFields = filter available fieldNames
  let mkPat n = do
        e <- newName (nameBase n)
        return ((n, VarP e), (Ident (nameBase n), VarE e))
  fmap unzip $ mapM mkPat remainingFields

-- Important note! reify will fail if the record type is defined in the
-- same module as the reify is used. This means quasi-quoted Hamlet
-- literals will not be able to use wildcards to match record types
-- defined in the same module.
recordToFieldNames :: DataConstr -> Q [Name]
recordToFieldNames conStr
  -- use 'lookupValueName' instead of just using 'mkName' so we reify the
  -- data constructor and not the type constructor if their names match.
 = do
  Just conName <- lookupValueName $ conToStr conStr
  DataConI _ _ typeName <- reify conName
  TyConI (DataD _ _ _ _ cons _) <- reify typeName
  [fields] <- return [fields | RecC name fields <- cons, name == conName]
  return [fieldName | (fieldName, _, _) <- fields]

type QueryParameters = [(Text, Text)]

data VarExp msg url
  = EPlain Html
  | EUrl url
  | EUrlParam (url, QueryParameters)
  | EMixin (HtmlUrl url)
  | EMixinI18n (HtmlUrlI18n msg url)
  | EMsg msg

instance Show (VarExp msg url) where
  show (EPlain _) = "EPlain"
  show (EUrl _) = "EUrl"
  show (EUrlParam _) = "EUrlParam"
  show (EMixin _) = "EMixin"
  show (EMixinI18n _) = "EMixinI18n"
  show (EMsg _) = "EMsg"
