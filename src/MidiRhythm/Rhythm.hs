module MidiRhythm.Rhythm where

import MidiRhythm.NotePress
import qualified Data.Vector as Vec

import qualified Numeric.NonNegative.Wrapper as NonNeg

import Data.List
import Data.Maybe

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

splitByTime :: ElapsedTime -> [NotePress] -> ([NotePress], [NotePress])
splitByTime time presses = case findIndex (\p -> getTime p >= time) presses of
  Just i -> splitAt i presses
  Nothing -> ([], presses)

splitByTime2 :: ElapsedTime -> ElapsedTime -> [NotePress] -> ([NotePress], [NotePress], [NotePress])
splitByTime2 t1 t2 presses
  | t1 > t2 = splitByTime2 t2 t1 presses
  | otherwise =
    let (before, afterFirst) = splitByTime t1 presses
        (beforeSnd, afterSnd) = splitByTime t2 afterFirst
    in (before, beforeSnd, afterSnd)

getTimeOffsets :: [NotePress] -> [ElapsedTime]
getTimeOffsets [] = []
getTimeOffsets ps@(first:_) = map (\p -> getTime p - getTime first) ps

getChunksWithRadius :: Duration -> [ElapsedTime] -> [NotePress] -> [[NotePress]]
getChunksWithRadius _ [] _ = []
getChunksWithRadius r (t:ts) ps =
  let (_, inside, after) = splitByTime2 (t - r) (t + r) ps
  in (inside : getChunksWithRadius r ts (inside ++ after))

getParallelChunks :: Duration -> Int -> Duration -> [NotePress] -> ([[NotePress]], [[NotePress]])
getParallelChunks r offset len ps1 =
  (getChunksWithRadius r (timesFrom ps1) ps1,
    getChunksWithRadius r (timesFrom ps2) ps2)
    where
      ps2 = drop offset ps1
      timeOffsets = sort $ takeWhile (< len) (getTimeOffsets ps1) ++
                          takeWhile (< len) (getTimeOffsets ps2)

      timesWithOffset :: Maybe ElapsedTime -> [ElapsedTime]
      timesWithOffset Nothing = []
      timesWithOffset (Just t) = map (+ t) timeOffsets

      timesFrom ps = timesWithOffset $ listToMaybe $ map getTime ps
