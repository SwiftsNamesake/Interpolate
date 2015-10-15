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

-- TODO | - Use variable-length (printf-style) interface, or a list
--        - Separate Arg logic from formatting (separate type classes)
--        - Use Existential Quantification (?)
--        - Clear up terminology
--        - Varargs composition
--        - Factor out internal functions (and move types to where they belong)
--        - Performance, profiling, QuickCheck

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
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

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Deal with (break out format args validation)
--        - Duplicate keys
--        - Missing keys
--        - Invalid formats
--        - Implicit indices
--        - Mixing implicit and explicit keys (disallowed)
-- interpolate :: (Parsec.Stream s Identity Char, IsString s, Monoid s, FormatArg arg) => s -> arg -> [s]
-- interpolate :: (Parsec.Stream s Identity Char, IsString s, Monoid s) => s -> [(Key, FormatWrapper)] -> [s]
interpolate :: [FormatToken] -> [(Key, FormatWrapper)] -> [Either FormatError String]
interpolate ftokens args = map tryformat ftokens
  where
    findvalue :: Key -> Maybe FormatWrapper
    findvalue k = M.lookup k (M.fromList args)

    tryformat :: FormatToken -> Either FormatError String
    tryformat (PlainToken s)             = Right s
    tryformat (SpecifierToken (k, spec)) = maybe (Left $ MissingKey k) (\(FormatWrapper v) -> format spec v) (findvalue k)
  --   tokens :: Either Parsec.ParseError [FormatToken]
  --   tokens = Parsec.parse parseformat "(source)" text
  --


-- |
-- TODO: Rename (eg. normalise) (?)
validateFormat :: [FormatToken] -> Either [FormatError] [FormatToken]
validateFormat = undefined


-- |
-- TODO: Rename (?)
validateArgs :: [(Key, FormatWrapper)] -> Either [FormatError] [FormatToken]
validateArgs = undefined


-- |
doformat :: FormatArg arg => String -> arg -> Either FormatError String
doformat = undefined


-- |
-- packargs ::


-- |
-- collectargs = _

-- Types (should be moved) -----------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Move
-- TODO: Remove, use existential quantification for heterogenous lists, or interpolate incrementally (?)
-- data FormatItem i s = IntegerItem s | StringItem s
-- TODO: Use (:=) operator instead (?)
-- data FormatWrapper = forall a k. (FormatKey k, FormatItem a) => FormatWrapper k a
data FormatWrapper = forall a. (FormatItem a, Show a) => FormatWrapper a


-- |
instance Show (FormatWrapper) where
  show (FormatWrapper s) = "FormatWrapper " ++ show s


-- | Varargs function composition. What have I gotten myself into...
-- TODO: Let 'collect' accept a function to pass the result into
-- TODO: Figure out a way to express the final type of a varargs function
(...) :: (FormatArg arg) => (a -> b) -> arg -> b
f ... g = undefined


-- |
-- TODO: Rename (?)
(=:=) :: (FormatItem a, Show a) => Key -> a -> (Key, FormatWrapper)
(=:=) key it = (key, FormatWrapper it)


-- |
class FormatItem a where
  -- TODO: Rename (?)
  format :: (IsString s) => Specifier -> a -> Either FormatError s
  wrap   :: a -> (Key, FormatWrapper)


-- | This class implements the varargs behaviour
class FormatArg a where
  collect :: [(Key, FormatWrapper)] -> a


-- Arg instances ---------------------------------------------------------------------------------------------------------------------------

-- |
-- instance FormatItem
-- instance (FormatItem a, Show a, FormatArg b) => FormatArg ((Key, a) -> b) where
  -- collect others arg = collect (wrap arg : others)


instance (FormatItem a, FormatArg b) => FormatArg (a -> b) where
  collect others arg = collect (wrap arg : others)


-- |
instance FormatArg [(Key, FormatWrapper)] where
  collect others = others

-- Item instances --------------------------------------------------------------------------------------------------------------------------

instance (FormatItem it, Show it) => FormatItem (Key, it) where
    wrap (key, item)   = (key, FormatWrapper item)
    format spec (_, it) = format spec it


instance FormatItem String where
  wrap item = (EmptyKey, FormatWrapper item)
  format spec s = Right $ fromString s


instance FormatItem Int where
  wrap item = (EmptyKey, FormatWrapper item)
  format spec i = Right . fromString $ show i
