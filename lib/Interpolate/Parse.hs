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
--                                              ↓                  ↓                   ↓                 ↓
parseformat :: (Stream s' Identity Char, Monoid s, IsString s) => ParsecT s' u Identity [FormatToken s i]
parseformat = Parsec.many (plain <|> specifier)


-- |
plain :: forall s' s u i. (Stream s' Identity Char, Monoid s, IsString s) => ParsecT s' u Identity (FormatToken s i)
plain = do
  s <- Parsec.many $ unescaped <|> openescape <|> closeescape -- TODO: Find a way of flattening
  return . PlainToken $ mconcat s


-- |
unescaped :: (Stream s' Identity Char, IsString s) => ParsecT s' u Identity s
unescaped = fromString <$> (:[]) <$> Parsec.noneOf "{}"


-- |
openescape :: (Stream s' Identity Char, IsString s) => ParsecT s' u Identity s -- [FormatToken s i]
openescape = generic $ Parsec.string "{{"


-- |
closeescape :: (Stream s' Identity Char, IsString s) => ParsecT s' u Identity s -- [FormatToken s i]
closeescape = generic $ Parsec.string "}}"


-- |
open = error ""


-- |
close = undefined


-- |
specifier :: IsString s => ParsecT s' u Identity (FormatToken s i)
specifier = do
  open
  (key, spec) <- undefined
  close
  return $ SpecifierToken (Specifier key spec)



-- |
-- generic ::
generic :: IsString s => ParsecT s' u Identity String -> ParsecT s' u Identity s
generic = (fromString <$>)
