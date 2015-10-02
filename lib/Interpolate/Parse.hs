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
--        - Empty formatting specifiers (?)

-- SPEC | -
--        -

-- cf. https://docs.python.org/3.4/library/string.html#formatspec



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Interpolate.Parse where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Text.Parsec as Parsec
import           Text.Parsec ((<?>), (<|>), ParsecT, Stream)

import Data.Functor ((<$>))
import Data.Monoid
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
parseformat :: (Stream s' Identity Char, Monoid s, FormatKey k, IsString s) => ParsecT s' u Identity [FormatToken s k]
parseformat = Parsec.many (plain <|> specifier)


-- |
plain :: (Stream s' Identity Char, Monoid s, FormatKey k, IsString s) => ParsecT s' u Identity (FormatToken s k)
plain = do
  s <- Parsec.many $ unescaped <|> openescape <|> closeescape -- TODO: Find a way of flattening
  return . PlainToken $ mconcat s


-- |
unescaped :: (Stream s' Identity Char, IsString s) => ParsecT s' u Identity s
unescaped = fromString <$> (:[]) <$> Parsec.noneOf "{}"


-- |
openescape :: (Stream s' Identity Char, IsString s) => ParsecT s' u Identity s
openescape = string "{{"


-- |
closeescape :: (Stream s' Identity Char, IsString s) => ParsecT s' u Identity s
closeescape = string "}}"


-- |
open :: (Stream s' Identity Char, IsString s) => ParsecT s' u Identity s
open = string "{"


-- |
close :: (Stream s' Identity Char, IsString s) => ParsecT s' u Identity s
close = string "}"


-- |
-- TOOD: Rename (?)
specifier :: (Stream s' Identity Char, FormatKey k, IsString s) => ParsecT s' u Identity (FormatToken s k)
specifier = do
  open
  (key, spec) <- formatspec
  close
  return $ SpecifierToken (Specifier key spec)


-- |
formatspec :: (Stream s' Identity Char, FormatKey k, IsString s) => ParsecT s' u Identity (s, k)
formatspec = do
  undefined
-- [[fill]align][sign][#][0][width][,][.precision][type]


-- |
-- generic ::
string :: (Stream s' Identity Char, IsString s) => String -> ParsecT s' u Identity s
string str = fromString <$> Parsec.string str
