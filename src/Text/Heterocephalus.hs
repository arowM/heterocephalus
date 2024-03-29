{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Text.Heterocephalus

Copyright   :  Kadzuya Okamoto 2016
License     :  MIT

Stability   :  experimental
Portability :  unknown

This module exports functions for working with frontend templates from Haskell.
-}

module Text.Heterocephalus
  (
  -- * Core functions
    compileTextFile
  , compileTextFileWith
  , compileTextFileWithDefault
  , compileHtmlFile
  , compileHtmlFileWith
  , compileHtmlFileWithDefault

  -- * QuasiQuoters
  , compileText
  , compileHtml

  -- * ScopeM
  , ScopeM
  , setDefault
  , overwrite

  -- * low-level
  , HeterocephalusSetting(..)
  , textSetting
  , htmlSetting
  , ParseOptions(..)
  , defaultParseOptions
  , createParseOptions
  , DefaultScope
  , compile
  , compileWith
  , compileWithDefault
  , compileFile
  , compileFileWith
  , compileFileWithDefault
  , compileFromString
  , compileFromStringWithDefault
  ) where

#if MIN_VERSION_base(4,9,0)
#else
import Control.Applicative ((<$>), (<*>), Applicative(..))
import Data.Monoid (Monoid, mempty, mappend)
#endif
import Control.Monad (forM)
import Data.Char (isDigit)
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Foldable as F
import Data.List (intercalate)
import qualified Data.Semigroup as Sem
import Data.String (IsString(..))
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import Language.Haskell.TH.Lib (ExpQ, varE)
import Language.Haskell.TH.Quote
       (QuasiQuoter(QuasiQuoter), quoteExp, quoteDec, quotePat, quoteType)
#if MIN_VERSION_template_haskell(2,9,0)
import Language.Haskell.TH.Syntax
       (Body(..), Con(..), Dec(..), Exp(..), Info(..), Lit(..), Match(..),
        Name(..), Pat(..), Q, Stmt(..), lookupValueName, mkName, nameBase,
        newName, qAddDependentFile, qRunIO, reify)
#else
import Language.Haskell.TH.Syntax
#endif
import TemplateHaskell.Compat.V0208
import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Internal (preEscapedText)
import Text.Hamlet (Html, HtmlUrl, HtmlUrlI18n, condH)
import Text.Hamlet.Parse
       (Binding(..), DataConstr(..), Module(Module), specialOrIdent)
import Text.Shakespeare.Base
       (Deref, Ident(..), Scope, derefToExp, readUtf8File)

import Text.Heterocephalus.Parse
       (Doc(..), Content(..), ParseOptions(..), createParseOptions,
        defaultParseOptions, docFromString)

{- $setup
  >>> :set -XTemplateHaskell -XQuasiQuotes
  >>> import Text.Blaze.Renderer.String
-}

{-| A function to compile template file.
  This function __DOES NOT__ escape template variables.
  To render the compiled file, use @'Text.Blaze.Renderer'.*.renderMarkup@.

  >>> putStr $ renderMarkup (let as = ["<a>", "b"] in $(compileTextFile "templates/sample.txt"))
  sample
  key: <a>,
  key: b,
 -}
compileTextFile :: FilePath -> Q Exp
compileTextFile = compileFile textSetting

{-| Same as 'compileText' but allows the user to specify extra values for template parameters.
  Values declared by 'overwrite' overwrites same name variables.
  Values declared by 'setDefault' are overwritten by same name variables.

>>> :set -XOverloadedStrings
>>> :{
putStr $ renderMarkup (
  let as = ["<a>", "b"]
  in $(compileTextFileWith "templates/sample.txt" $ do
    setDefault "as" [| ["foo", "bar"] |]
  )
)
:}
sample
key: <a>,
key: b,

>>> :{
putStr $ renderMarkup (
  let as = ["<a>", "b"]
  in $(compileTextFileWith "templates/sample.txt" $ do
    overwrite "as" [| ["foo", "bar"] |]
  )
)
:}
sample
key: foo,
key: bar,

>>> :{
putStr $ renderMarkup (
  let as = ["<a>", "b"]
  in $(compileTextFileWith "templates/sample.txt" $ do
    overwrite "as" [| ["bazbaz", "barbar"] |]
    setDefault "as" [| ["foo", "bar"] |]
    overwrite "as" [| ["baz", "foobar"] |]
  )
)
:}
sample
key: baz,
key: foobar,
 -}
compileTextFileWith :: FilePath -> ScopeM () -> Q Exp
compileTextFileWith fp scopeM = compileFileWith scopeM textSetting fp

{-| Same as 'compileText' but allows the user to specify default values for template parameters.

>>> :set -XOverloadedStrings
>>> :{
putStr $ renderMarkup (
  let as = ["<a>", "b"]
  in $(compileTextFileWithDefault "templates/sample.txt"
    [("as", [| ["foo", "bar"] |])]
  )
)
:}
sample
key: <a>,
key: b,

>>> :{
putStr $ renderMarkup (
  $(compileTextFileWithDefault "templates/sample.txt"
    [("as", [| ["foo", "bar"] |])]
  )
)
:}
sample
key: foo,
key: bar,
 -}
compileTextFileWithDefault :: FilePath -> DefaultScope -> Q Exp
compileTextFileWithDefault fp scope = compileFileWithDefault scope textSetting fp

{-| Same as 'compileTextFile' but escapes template variables in HTML.

  >>> putStr $ renderMarkup (let as = ["<a>", "b"] in $(compileHtmlFile "templates/sample.txt"))
  sample
  key: &lt;a&gt;,
  key: b,
 -}
compileHtmlFile :: FilePath -> Q Exp
compileHtmlFile fp = compileHtmlFileWithDefault fp []

{-| Same as 'compileHtmlFile' but allows the user to specify extra values for template parameters.
  Values declared by 'overwrite' overwrites same name variables.
  Values declared by 'setDefault' are overwritten by same name variables.

>>> :set -XOverloadedStrings
>>> :{
putStr $ renderMarkup (
  let as = ["<a>", "b"]
  in $(compileHtmlFileWith "templates/sample.txt" $ do
    setDefault "as" [| ["foo", "bar"] |]
  )
)
:}
sample
key: &lt;a&gt;,
key: b,

>>> :{
putStr $ renderMarkup (
  let as = ["<a>", "b"]
  in $(compileHtmlFileWith "templates/sample.txt" $ do
    overwrite "as" [| ["foo", "bar"] |]
  )
)
:}
sample
key: foo,
key: bar,

>>> :{
putStr $ renderMarkup (
  let as = ["<a>", "b"]
  in $(compileHtmlFileWith "templates/sample.txt" $ do
    overwrite "as" [| ["bazbaz", "barbar"] |]
    setDefault "as" [| ["foo", "bar"] |]
    overwrite "as" [| ["baz", "foobar"] |]
  )
)
:}
sample
key: baz,
key: foobar,
 -}
compileHtmlFileWith :: FilePath -> ScopeM () -> Q Exp
compileHtmlFileWith fp scopeM = compileFileWith scopeM htmlSetting fp

{-| Same as 'compileHtmlFile' but allows the user to specify default values for template parameters.

>>> :set -XOverloadedStrings
:{
putStr $ renderMarkup (
  let as = ["<a>", "b"]
  in $(compileHtmlFileWithDefault "templates/sample.txt"
    [("as", [| ["foo", "bar"] |])]
  )
)
:}
sample
key: &lt;a&gt;,
key: b,

>>> :{
putStr $ renderMarkup (
  $(compileHtmlFileWithDefault "templates/sample.txt"
    [("as", [| ["foo", "bar"] |])]
  )
)
:}
sample
key: foo,
key: bar,
 -}
compileHtmlFileWithDefault :: FilePath -> DefaultScope -> Q Exp
compileHtmlFileWithDefault fp scope = compileFileWithDefault scope htmlSetting fp

{-| Heterocephalus quasi-quoter.
  This function __DOES NOT__ escape template variables.
  To render the compiled file, use @'Text.Blaze.Renderer'.*.renderMarkup@.

  >>> renderMarkup (let as = ["<a>", "b"] in [compileText|sample %{ forall a <- as }key: #{a}, %{ endforall }|])
  "sample key: <a>, key: b, "

  >>> renderMarkup (let num=2 in [compileText|#{num} is %{ if even num }an even number.%{ elseif (num > 100) }big.%{ else }an odd number.%{ endif }|])
  "2 is an even number."
 -}
compileText :: QuasiQuoter
compileText = compile textSetting

{-| Heterocephalus quasi-quoter for HTML.
  Same as 'compileText' but this function does escape template variables in HTML.

  >>> renderMarkup (let as = ["<a>", "b"] in [compileHtml|sample %{ forall a <- as }key: #{a}, %{ endforall }|])
  "sample key: &lt;a&gt;, key: b, "
 -}
compileHtml :: QuasiQuoter
compileHtml = compile htmlSetting

compile :: HeterocephalusSetting -> QuasiQuoter
compile = compileWithDefault []

{-| QuasiQuoter.
 -}
compileWith :: ScopeM () -> HeterocephalusSetting -> QuasiQuoter
compileWith scopeM set =
  QuasiQuoter
  { quoteExp = compileFromStringWith scopeM set
  , quotePat = error "not used"
  , quoteType = error "not used"
  , quoteDec = error "not used"
  }

{-| QuasiQuoter.
 -}
compileWithDefault :: DefaultScope -> HeterocephalusSetting -> QuasiQuoter
compileWithDefault scope set =
  QuasiQuoter
  { quoteExp = compileFromStringWithDefault scope set
  , quotePat = error "not used"
  , quoteType = error "not used"
  , quoteDec = error "not used"
  }

{-| Compile a template file.
-}
compileFile :: HeterocephalusSetting -> FilePath -> Q Exp
compileFile = compileFileWithDefault []

{-| Same as 'compileFile' but we can specify default scope.
-}
compileFileWith :: ScopeM () -> HeterocephalusSetting -> FilePath -> Q Exp
compileFileWith scopeM set fp = do
  qAddDependentFile fp
  contents <- fmap TL.unpack $ qRunIO $ readUtf8File fp
  compileFromStringWith scopeM set contents

{-| Same as 'compileFile' but we can specify default scope.
-}
compileFileWithDefault :: DefaultScope -> HeterocephalusSetting -> FilePath -> Q Exp
compileFileWithDefault scope' set fp = do
  qAddDependentFile fp
  contents <- fmap TL.unpack $ qRunIO $ readUtf8File fp
  compileFromStringWithDefault scope' set contents

{-| Same as 'compileFile', but just compile the 'String' given.

  >>> let as = ["<a>", "b"]
  >>> let template = "sample %{ forall a <- as }key: #{a}, %{ endforall }"
  >>> renderMarkup $(compileFromString textSetting template)
  "sample key: <a>, key: b, "

  >>> let as = ["<a>", "b"]
  >>> let options = createParseOptions '|' '?'
  >>> let setting = textSetting { parseOptions = options }
  >>> let template = "sample |{ forall a <- as }key: ?{a}, |{ endforall }"
  >>> renderMarkup $(compileFromString setting template)
  "sample key: <a>, key: b, "
-}
compileFromString :: HeterocephalusSetting -> String -> Q Exp
compileFromString = compileFromStringWithDefault []

compileFromStringWith :: ScopeM () -> HeterocephalusSetting -> String -> Q Exp
compileFromStringWith scopeM set s = do
  defScope' <-
    forM defScope $ \(ident, qexp) -> (ident, ) <$> overwriteScope ident qexp
  owScope' <-
    forM owScope $ \(ident, qexp) -> (ident, ) <$> qexp
  docsToExp set (owScope' ++ defScope') $ docFromString (parseOptions set) s
 where
  (defDList, owDList) = runScopeM scopeM
  defScope = DList.toList defDList
  owScope = DList.toList owDList

compileFromStringWithDefault :: DefaultScope -> HeterocephalusSetting -> String -> Q Exp
compileFromStringWithDefault scope' set s = do
  scope <-
    forM scope' $ \(ident, qexp) -> (ident, ) <$> overwriteScope ident qexp
  docsToExp set scope $ docFromString (parseOptions set) s

overwriteScope :: Ident -> Q Exp -> Q Exp
overwriteScope (Ident str) qexp = do
  mName <- lookupValueName str
  case mName of
    Just x -> varE x
    Nothing -> qexp

{-| Settings that are used when processing heterocephalus templates.
 -}
data HeterocephalusSetting = HeterocephalusSetting
  { escapeExp :: Q Exp
  -- ^ Template variables are passed to 'escapeExp' in the output.  This allows
  -- things like escaping HTML entities.  (See 'htmlSetting'.)
  , parseOptions :: ParseOptions
  }

{-| A setting that escapes template variables for Html

This sets 'escapeExp' to 'toHtml'.
 -}
htmlSetting :: HeterocephalusSetting
htmlSetting = HeterocephalusSetting
  { escapeExp = [|toHtml|]
  , parseOptions = defaultParseOptions
  }

{-| A setting that DOES NOT escape template variables.

This sets 'escapeExp' to 'preEscapedToMarkup'.
 -}
textSetting :: HeterocephalusSetting
textSetting = HeterocephalusSetting
  { escapeExp = [|preEscapedToMarkup|]
  , parseOptions = defaultParseOptions
  }

type DefaultScope = [(Ident, Q Exp)]

type DefaultDList = DList (Ident, Q Exp)
type OverwriteDList = DList (Ident, Q Exp)

{- | A type to handle extra scopes.
   This is opaque type, so use 'setDefault' and 'overwrite'
   to construct new 'ScopeM'.
 -}
data ScopeM a
  = SetDefault Ident ExpQ (ScopeM a)
  | Overwrite Ident ExpQ (ScopeM a)
  | PureScopeM a

{- | Get default values and values to overwrite from 'ScopeM'.
 -}
runScopeM :: ScopeM a -> (DefaultDList, OverwriteDList)
runScopeM (SetDefault ident qexp next) =
  let (defaults, overwrites) = runScopeM next
  in (DList.snoc defaults (ident, qexp), overwrites)
runScopeM (Overwrite ident qexp next) =
  let (defaults, overwrites) = runScopeM next
  in (defaults, DList.snoc overwrites (ident, qexp))
runScopeM (PureScopeM _) =
  (mempty, mempty)

instance Sem.Semigroup (ScopeM ()) where
  a <> b  = a >> b

instance Monoid (ScopeM ()) where
  mempty = pure ()
#if !(MIN_VERSION_base(4,11,0))
  mappend = (Sem.<>)
#endif

instance Functor ScopeM where
  fmap f (SetDefault ident qexp next) =
    SetDefault ident qexp $ fmap f next
  fmap f (Overwrite ident qexp next) =
    Overwrite ident qexp $ fmap f next
  fmap f (PureScopeM x) =
    PureScopeM $ f x

instance Applicative ScopeM where
  pure = PureScopeM
  SetDefault ident qexp next <*> f =
    SetDefault ident qexp $ next <*> f
  Overwrite ident qexp next <*> f =
    Overwrite ident qexp $ next <*> f
  PureScopeM g <*> f = f >>= (PureScopeM . g)

instance Monad ScopeM where
#if MIN_VERSION_base(4,9,0)
#else
  return = PureScopeM
#endif
  SetDefault ident qexp next >>= f = SetDefault ident qexp $ next >>= f
  Overwrite ident qexp next >>= f = Overwrite ident qexp $ next >>= f
  PureScopeM a >>= f = f a

{-| Constructor for 'ScopeM'.
  Values declared by this function are overwritten
  by same name variables exits in scope of render function.
 -}
setDefault :: Ident -> Q Exp -> ScopeM ()
setDefault ident qexp = SetDefault ident qexp $ pure ()

{-| Constructor for 'ScopeM'.
  Values declared by this function overwrites
  same name variables exits in scope of render function.
 -}
overwrite :: Ident -> Q Exp -> ScopeM ()
overwrite ident qexp = Overwrite ident qexp $ pure ()

instance IsString Ident where
  fromString = Ident

-- ==============================================
--  Helper functions
-- ==============================================

docsToExp :: HeterocephalusSetting -> Scope -> [Doc] -> Q Exp
docsToExp set scope docs = do
  exps <- mapM (docToExp set scope) docs
  case exps of
    [] -> [|return ()|]
    [x] -> return x
    _ -> return $ doE $ map NoBindS exps

docToExp :: HeterocephalusSetting -> Scope -> Doc -> Q Exp
docToExp set scope (DocForall list idents inside) = do
  let list' = derefToExp scope list
  (pat, extraScope) <- bindingPattern idents
  let scope' = extraScope ++ scope
  mh <- [|F.mapM_|]
  inside' <- docsToExp set scope' inside
  let lam = LamE [pat] inside'
  return $ mh `AppE` lam `AppE` list'
docToExp set scope (DocCond conds final) = do
  conds' <- mapM go conds
  final' <-
    case final of
      Nothing -> [|Nothing|]
      Just f -> do
        f' <- docsToExp set scope f
        j <- [|Just|]
        return $ j `AppE` f'
  ch <- [|condH|]
  return $ ch `AppE` ListE conds' `AppE` final'
  where
    go :: (Deref, [Doc]) -> Q Exp
    go (d, docs) = do
      let d' = derefToExp ((specialOrIdent, VarE 'or) : scope) d
      docs' <- docsToExp set scope docs
      return $ nonUnaryTupE [d', docs']
      where
        nonUnaryTupE :: [Exp] -> Exp
        nonUnaryTupE es  = tupE es
docToExp set scope (DocCase deref cases) = do
    let exp_ = derefToExp scope deref
    matches <- mapM toMatch cases
    return $ CaseE exp_ matches
  where
    toMatch :: (Binding, [Doc]) -> Q Match
    toMatch (idents, inside) = do
        (pat, extraScope) <- bindingPattern idents
        let scope' = extraScope ++ scope
        insideExp <- docsToExp set scope' inside
        return $ Match pat (NormalB insideExp) []
docToExp set v (DocContent c) = contentToExp set v c

contentToExp :: HeterocephalusSetting -> Scope -> Content -> Q Exp
contentToExp _ _ (ContentRaw s) = do
  os <- [|preEscapedText . pack|]
  let s' = LitE $ StringL s
  return $ os `AppE` s'
contentToExp set scope (ContentVar d) = do
  str <- escapeExp set
  return $ str `AppE` derefToExp scope d

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
#if MIN_VERSION_template_haskell_compat_v0208(0,1,8)
  {- 
  Todo: Note, the use of conP below (note lowercase 'c') is from the package 'template-haskell-compat-v0208'.
  This allows Heterocephalus to compile with GHC 9.2, as the ConP constructor takes an additional argument in GHC 9.2
  Note however this _probably_ means there was some new extension or new syntax extension that GHC now allows post GHC 9.2
  that we are not able to handle. As I (clintonmead@gmail.com) am just writing a PR to the library and don't fully understand it
  (I'm making this PR just to make our current project build under GHC 9.2) there's probably some more work to be done here.

  The github issue relating to this is at https://github.com/arowM/heterocephalus/issues/32
  -}
  return (conP (mkConName con) patterns, concat scopes)
#else
  return (ConP (mkConName con) patterns, concat scopes)
#endif
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
#if MIN_VERSION_template_haskell(2,11,0)
  DataConI _ _ typeName         <- reify conName
  TyConI (DataD _ _ _ _ cons _) <- reify typeName
#else
  DataConI _ _ typeName _     <- reify conName
  TyConI (DataD _ _ _ cons _) <- reify typeName
#endif
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
