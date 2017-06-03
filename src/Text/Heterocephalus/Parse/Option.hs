{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Heterocephalus.Parse.Option where

#if MIN_VERSION_base(4,9,0)
#else
import Control.Applicative ((<$>), (*>), (<*), pure)
#endif

import Control.Monad.Reader (MonadReader, reader)

data ParseOptions = ParseOptions
  { parseOptionsControlPrefix :: Char
  , parseOptionsVariablePrefix :: Char
  }

-- | Default set of parser options.
--
-- Sets 'parseOptionsControlPrefix' to @\'%\'@  and
-- 'parseOptionsVariablePrefix' to @\'#\'@.
defaultParseOptions :: ParseOptions
defaultParseOptions = createParseOptions '%' '#'

createParseOptions
  :: Char  -- ^ The control prefix.
  -> Char  -- ^ The variable prefix.
  -> ParseOptions
createParseOptions controlPrefix varPrefix = ParseOptions
  { parseOptionsControlPrefix = controlPrefix
  , parseOptionsVariablePrefix = varPrefix
  }

getControlPrefix :: MonadReader ParseOptions m => m Char
getControlPrefix = reader parseOptionsControlPrefix

getVariablePrefix :: MonadReader ParseOptions m => m Char
getVariablePrefix = reader parseOptionsVariablePrefix
