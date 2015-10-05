-- |
-- Module      : Interpolate.Types
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created September 30 2015

-- TODO | - Deriving, templates (?)
--        - Expose key class or lock to string keys and indices (?)
--        - Use lenses and records (?)

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Interpolate.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.String



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- Types -----------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Rename (?)
data FormatToken i s = PlainToken s | SpecifierToken (Key i s, Specifier s) deriving (Show)


-- |
data Key i s = StringKey s  | IndexKey i | EmptyKey deriving (Show)


-- |
-- TODO: Figure out how to represent format specs
data Specifier s = Specifier s deriving (Show)


-- |
type Format k s = (Key k s, Specifier s)

--------------------------------------------------------------------------------------------------------------------------------------------

-- type TokensParser  = ParsecT String () Identity [FormatToken String Int]
-- type TokenParser   = ParsecT String () Identity (FormatToken String Int)
-- type Parser a      = ParsecT String () Identity a
-- type ParseResult a = Either Parsec.ParseError a

-- Classes ---------------------------------------------------------------------------------------------------------------------------------

-- |
class FormatArg arg where
  interpolate :: IsString str => str -> (str, arg)


-- |
class FormatKey key where
  match :: String -> key -> Bool
  -- match :: IsString str => str -> key -- TODO: Rename, write proper signatures

-- Instances -------------------------------------------------------------------------------------------------------------------------------

-- |
instance FormatKey [Char] where
  match = (==)
  -- match :: IsString str => str -> str -> Bool
  -- match = (==)


-- |
instance FormatKey Int where
  match = flip $ (==) . show
  -- match key i = key == show i
