{-# LANGUAGE ScopedTypeVariables #-}
module MidiRhythm.Rhythm where

import Control.Arrow
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

avgPress :: [NotePress] -> NotePress
avgPress presses = NotePress
                    (avg getTime)
                    (avg getVelocity)
                    (avg getDuration)
                    (avg getPitch)
  where
    avg getX = sum (map getX presses) `div` fromIntegral (length presses)

chordDiff :: Chord -> Chord -> ChordDiff
chordDiff (Chord ps1) (Chord ps2) =
    avgPressDiff (avgWithCount ps1) (avgWithCount ps2)
  where
    avgPressDiff (NotePress _ vel1 dur1 pt1, ct1)
                (NotePress _ vel2 dur2 pt2, ct2) =
                  ChordDiff (pitchDiff pt1 pt2)
                              (velocityDiff vel1 vel2)
                              (durationDiff dur1 dur2)
                              (pressCountDiff ct1 ct2)

    avgWithCount :: [NotePress] -> (NotePress, PressCount)
    avgWithCount presses = (avgPress presses,
      PressCount (NonNeg.fromNumberUnsafe $ length presses))

getTimeOffsets :: [NotePress] -> [ElapsedTime]
getTimeOffsets [] = []
getTimeOffsets ps@(first:_) = map (\p -> getTime p - getTime first) ps


data BarSide = LeftBar | RightBar
  deriving (Eq)

data SuperimposedInfo t = SuperimposedInfo {
  barSide :: BarSide,
  barScaledTime :: t
}


-- todo: bin search
splitByThreshold :: (Ord t) => (a -> t) -> t -> [a] -> ([a], [a])
splitByThreshold timeFn threshold xs =
  case findIndex (\p -> timeFn p >= threshold) xs of
    Just i -> splitAt i xs
    Nothing -> (xs, [])

splitByTimes :: (Ord t) => (a -> t) -> [t] -> [a] -> [[a]]
splitByTimes time [] presses = [presses]
splitByTimes time (t:ts) presses = p1 : splitByTimes time ts p2
  where (p1, p2) = splitByThreshold time t presses

getSuperimposedChunks :: forall a t. Integral t
                  => (a -> t) -- time getter
                  -> t -- start of first bar
                  -> t -- first bar length
                  -> t -- second bar length
                  -> NonNeg.Integer -- granularity (part of a bar considered instantaneous)
                  -> [a] -- music to split
                  -> [([a], [a])]
getSuperimposedChunks getT start firstDur secondDur gran ps =
  map partitionToPresses mergedChunks
  where
    [_, firstBar, secondBar, _] = splitByTimes getT
            [start, start + firstDur, start + firstDur + secondDur] ps

    scale :: t -> t -> t -> t
    scale start mult t = (t - start) * mult

    -- to the common denominator
    scaledTs1 :: [t]
    scaledTs1 = map (scale start secondDur . getT) firstBar

    scaledTs2 :: [t]
    scaledTs2 = map (scale (start + firstDur) firstDur . getT) secondBar

    mergedPresses :: [(a, SuperimposedInfo t)]
    mergedPresses =
      sortOn (barScaledTime . snd) $
      firstBar `zip` map (SuperimposedInfo LeftBar ) scaledTs1 ++
      secondBar `zip` map (SuperimposedInfo RightBar ) scaledTs2

    chunkStep :: t
    chunkStep = (firstDur * secondDur) `div` fromIntegral gran

    steppedTimes :: [t]
    steppedTimes = tail [0, chunkStep .. firstDur * secondDur - 1]

    mergedChunks :: [[(a, SuperimposedInfo t)]]
    mergedChunks = splitByTimes (barScaledTime . snd)
                                steppedTimes mergedPresses

    partitionToPresses :: [(a, SuperimposedInfo t)]
                       -> ([a], [a])
    partitionToPresses chunk =
      let (left, right) =
            partition (\(_, i) -> barSide i == LeftBar) chunk
      in (map fst left, map fst right)
