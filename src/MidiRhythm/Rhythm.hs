module MidiRhythm.Rhythm where

import MidiRhythm.NotePress
import qualified Data.Vector as Vec

import qualified Numeric.NonNegative.Wrapper as NonNeg


newtype PitchDiff = PitchDiff Int
newtype VelocityDiff = VelocityDiff Int
newtype DurationDiff = DurationDiff Int
newtype PressCountDiff = PressCountDiff Int


diffInt x y = fromIntegral x - fromIntegral y

pitchDiff :: Pitch -> Pitch -> PitchDiff
pitchDiff x y = PitchDiff (diffInt x y)

velocityDiff :: Velocity -> Velocity -> VelocityDiff
velocityDiff x y = VelocityDiff (diffInt x y)

durationDiff :: Duration -> Duration -> DurationDiff
durationDiff x y = DurationDiff (diffInt x y)

pressCountDiff :: PressCount -> PressCount -> PressCountDiff
pressCountDiff x y = PressCountDiff (diffInt x y)

data ChordDiff = ChordDiff {
                    getPitchDiff :: PitchDiff,
                    getVelocityDiff :: VelocityDiff,
                    getDurationDiff :: DurationDiff,
                    getPressCountDiff :: PressCountDiff
                }

newtype Chord = Chord [NotePress] deriving Show

average :: Chord -> (NotePress, PressCount)
average (Chord presses) = (avgPress presses, PressCount (NonNeg.fromNumberUnsafe $ length presses))

avgPress :: [NotePress] -> NotePress
avgPress presses = NotePress (avg getTime) (avg getVelocity) (avg getDuration) (avg getPitch)
  where
    avg select = sum (map select presses) `div` fromIntegral (length presses)

pressDiff :: (NotePress, PressCount) -> (NotePress, PressCount) -> ChordDiff
pressDiff (NotePress _ vel1 dur1 pt1, ct1)
          (NotePress _ vel2 dur2 pt2, ct2) =
            ChordDiff (pitchDiff pt1 pt2)
                        (velocityDiff vel1 vel2)
                        (durationDiff dur1 dur2)
                        (pressCountDiff ct1 ct2)

tails :: [t] -> [[t]]
tails list = list : tails' list []
  where
    tails' [] acc = acc
    tails' (x:xs) acc = tails' xs (xs:acc)

pressesToChunks :: Duration -> [NotePress] -> [[NotePress]]
pressesToChunks delta presses = map (takeFirst delta) (tails presses)
  where
    takeFirst :: Duration -> [NotePress] -> [NotePress]
    takeFirst _ [] = []
    takeFirst delta presses =
      takeWhile (\p -> getTime p < delta + getTime (head presses)) presses
