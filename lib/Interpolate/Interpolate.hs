-- |
-- Module      : Interpolate.Interpolate
-- Description : Defines the machinery for applying formatting based on specifiers and interpolating the format string
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 1 2015

-- TODO | - Use variable-length (printf-style) interface, or
--        - Separate Arg logic from formatting (separate type classes)
--        - Use Existential Quantification (?)

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Interpolate.Interpolate where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Text.Parsec as Parsec

import Control.Monad.Identity

import Data.Monoid
import Data.String
import qualified Data.Map as M

import Interpolate.Types
import Interpolate.Parse (parseformat)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
interpolate :: (Parsec.Stream s Identity Char, IsString s, Monoid s) => s -> [s]
interpolate text = undefined
  where
    tokens :: Either Parsec.ParseError [FormatToken Int String]
    tokens = Parsec.parse parseformat "(source)" text


-- |
-- collectargs = _


-- |
-- format ::
-- format


-- |
-- TODO: Move
-- TODO: Remove, use existential quantification for heterogenous lists, or interpolate incrementally (?)
-- data FormatItem i s = IntegerItem s | StringItem s
-- TODO: Use (:=) operator instead (?)
data FormatWrapper = forall a k. (FormatKey k, FormatItem a) => FormatWrapper k a


class FormatItem a where
  -- TODO: Rename (?)
  format :: (IsString s) => Specifier s -> a -> s
  wrap   :: a -> FormatWrapper


class FormatArg a where
  -- TODO: Rename
  -- format :: String -> a -> String
  -- collect :: (FormatItem a, FormatArg b) => [FormatWrapper] -> a -> b
  collect :: [FormatWrapper] -> a


-- instance (Show s, Arg b) => Arg ((->) s b) where
--   apply' frmt i = apply' (frmt ++ show i)


-- instance FormatItem
instance (FormatItem a, FormatArg b) => FormatArg (a -> b) where
  collect others arg = collect (wrap arg : others)


instance FormatArg [FormatWrapper] where
  collect others = others


-- instance (Arg b) => Arg (String -> b) where
  -- apply' frmt s = apply' (frmt ++ s)


-- instance (Arg a, Arg b) => Arg ((->) a b) where
--   apply' frmt a = apply' (format frmt a) a
--   format = const


-- instance Arg Int where
  -- format frmt i = frmt ++ show i


-- instance Arg String where
  -- apply' :: Arg b => String -> String -> b
  -- apply' frmt = frmt
  -- format frmt s = frmt ++ s
