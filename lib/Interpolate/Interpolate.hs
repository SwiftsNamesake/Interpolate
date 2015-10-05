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
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Interpolate.Interpolate where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
-- import Interpolate.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- interpolate ::
-- interpolate =


-- |
-- format ::
-- format

data Angle f = Degrees f | Radians f deriving (Show)

withunits f (Degrees α) (Degrees β) = Degrees $ f α β
withunits f (Radians α) (Radians β) = Radians $ f α β
withunits f (Degrees α) (Radians β) = Degrees $ f α (β*180.0/pi)
withunits f (Radians α) (Degrees β) = Radians $ f α (β*pi/180.0)



-- (fromInteger 5) Degrees
-- Integer -> (Integer -> Angle f) -> Angle f
-- Black magic
instance Num n => Num ((Integer -> Angle n) -> Angle n) where
  fromInteger i = ($ i)

instance Functor Angle where
  fmap f (Degrees d) = Degrees $ f d
  fmap f (Radians r) = Radians $ f r

instance Floating f => Num (Angle f) where

  (+) = withunits (+)
  (-) = withunits (-)
  (*) = withunits (*)

  abs    = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = Degrees . fromInteger
  -- [abs, negate, signum] = map fmap [abs, negate, signum]



class Arg a where
  apply' :: String -> a
  -- format :: String -> a -> String


-- instance (Show s, Arg b) => Arg ((->) s b) where
--   apply' frmt i = apply' (frmt ++ show i)


instance (Arg b) => Arg ((->) Int b) where
  apply' frmt i = apply' (frmt ++ show i)


instance (Arg b) => Arg ((->) String b) where
  apply' frmt s = apply' (frmt ++ s)


-- instance (Arg a, Arg b) => Arg ((->) a b) where
--   apply' frmt a = apply' (format frmt a) a
--   format = const


-- instance Arg Int where
  -- format frmt i = frmt ++ show i


instance Arg String where
  -- apply' :: Arg b => String -> String -> b
  apply' frmt = frmt
  -- format frmt s = frmt ++ s
