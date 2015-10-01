-- |
-- Module      : Interpolate.Parse
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 30 2015

-- TODO | - Allow nested formats (?)
--        - Allow interpolation of format parameters (?)
--        - Polymorphic in String
--        - Allow arbitrary monad transformers
--        - Devise some way of specifying what parameters are valid for anyg iven FormatArg type
--          -- Type-specific specifiers

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Interpolate.Parse where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Text.Parsec as Parsec
import           Text.Parsec ((<?>), (<|>), ParsecT)
import Data.String
import Control.Monad.Identity

import Interpolate.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- parseformat :: IsString string => string -> a
-- ParsecT s u m a is a parser with stream type s, user state type u, underlying monad m and return type a.
-- TODO: Escaping '}'
--                                              ↓                  ↓                   ↓                 ↓
parseformat :: IsString s => ParsecT            s                  u               Identity        [FormatToken s i]
parseformat = do
  Parsec.many (plain <|> specifier)


-- |
plain :: IsString s => ParsecT s u Identity [FormatToken s i]
plain = Parsec.noneOf "{}" <|> fmap fromString (Parsec.string "{{") <|> fmap fromString (Parsec.string "}}")

-- |
specifier :: IsString s => ParsecT s u Identity [FormatToken]
specifier = error ""
