{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
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
import GHC.Generics

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
  notePressTime :: ElapsedTime,
  notePressVelocity :: Velocity,
  notePressDuration :: Duration,
  notePressPitch :: Pitch
} deriving (Show, Eq, Ord, Generic)
