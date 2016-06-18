{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MidiRhythm.NotePress (
    Duration(..),
    Press(..),
    NotePress(..),
    Velocity(..),
    Pitch(..),
    ElapsedTime(..),
    PressCount(..),
) where

import qualified Numeric.NonNegative.Wrapper as NonNeg
newtype ElapsedTime = ElapsedTime NonNeg.Integer
  deriving (Show, Eq, Ord, Num, Integral, Real, Enum)

newtype Velocity = Velocity NonNeg.Int
  deriving (Show, Eq, Ord, Num, Integral, Real, Enum)

type Duration = ElapsedTime

newtype Pitch = Pitch NonNeg.Int
  deriving (Show, Eq, Ord, Num, Integral, Real, Enum)

newtype PressCount = PressCount NonNeg.Int
    deriving (Show, Eq, Ord, Num, Integral, Real, Enum)

data Press = Press ElapsedTime Velocity Duration deriving (Show, Eq, Ord)

data NotePress = NotePress {
  getTime :: ElapsedTime,
  getVelocity :: Velocity,
  getDuration :: Duration,
  getPitch :: Pitch
} deriving (Show, Eq, Ord)
