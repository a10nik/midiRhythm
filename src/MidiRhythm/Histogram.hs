{-# LANGUAGE DeriveGeneric #-}
module MidiRhythm.Histogram where

import MidiRhythm.NotePress
import MidiRhythm.Rhythm
import Data.List
import Data.Ord
import qualified Data.Map.Strict as Map
import qualified Numeric.NonNegative.Wrapper as NonNeg
import Numeric.NonNegative.Class hiding (maximum)
import Control.Arrow
import Control.Monad.State
import GHC.Generics
import qualified Data.Set as Set

-- movingWindow :: ElapsedTime -> [NotePress] -> [[NotePress]]
-- movingWindow len presses = scanl'

noteOnDifferences :: [NotePress] -> [ElapsedTime]
noteOnDifferences presses =
  [fromIntegral $ abs (t1 - t2) | t1 <- times, t2 <- times]
  where
    times :: [Int]
    times = map (fromIntegral . notePressTime) presses

data GroupCount = GroupCount {
  groupLeftBoundary :: ElapsedTime,
  groupRightBoundary :: ElapsedTime,
  groupWeight :: NonNeg.Double
} deriving (Generic)

data NoteSimilarityCoeffs = NoteSimilarityCoeffs {
  velocitySimilarityCoeff :: NonNeg.Double,
  pitchSimilarityCoeff :: NonNeg.Double,
  durationSimilarityCoeff :: NonNeg.Double
} deriving (Show, Eq)

noteHistogram :: NoteSimilarityCoeffs -> ElapsedTime -> NonNeg.Int -> [NotePress] -> [GroupCount]
noteHistogram (NoteSimilarityCoeffs velC pitC durC) maxBarLen slices presses =
    map
      (\(gI, val) -> GroupCount (gI * sliceLen) ((gI + 1) * sliceLen) val)
      (Map.toAscList groups)
  where
    takeUpToMaxBarLength :: [NotePress] -> [NotePress]
    takeUpToMaxBarLength [] = []
    takeUpToMaxBarLength (first : ps) =
      takeWhile (\p -> notePressTime p < notePressTime first + maxBarLen) ps

    closeEnoughPresses :: [(NotePress, [NotePress])]
    closeEnoughPresses = presses `zip` map takeUpToMaxBarLength (tails presses)

    timeOffsetsWithSignificances :: [(ElapsedTime, NonNeg.Double)]
    timeOffsetsWithSignificances =
      [ (notePressTime p2 - notePressTime p1, relDist p1 p2 nextPresses)
      | (p1, nextPresses) <- closeEnoughPresses, p2 <- nextPresses ]

    diff :: (Integral a, Num b) => a -> a -> b
    diff i1 i2 = fromIntegral (if i1 < i2 then i2 - i1 else i1 - i2)

    relDist p1 p2 nextPresses =
      dist p1 p2 / Data.List.maximum (map (dist p1) nextPresses)

    dist (NotePress _ v1 d1 p1) (NotePress _ v2 d2 p2) =
      velC * diff v1 v2 + pitC * diff p1 p2 + durC * diff d1 d2

    sliceLen :: ElapsedTime
    sliceLen = maxBarLen `div` fromIntegral slices

    groups :: Map.Map ElapsedTime NonNeg.Double
    groups = Map.fromListWith (+)
      [(t `div` sliceLen, sgn) | (t, sgn) <- timeOffsetsWithSignificances]

data MovingRange t = MovingRange {
  movingRangeTime :: ElapsedTime,
  movingRangeMin :: Maybe t,
  movingRangeMax :: Maybe t
} deriving (Generic, Eq, Show)

movingRange :: Real t => (NotePress -> t) -> ElapsedTime -> [NotePress] -> [MovingRange t]
movingRange getX window presses =
    map (\(set, t) -> MovingRange t
      (set `ifSmth` minimum) (set `ifSmth` maximum)) xSets
  where
    ifSmth set f = if Set.null set then Nothing else Just (f set)
    xs = map getX presses
    times = map notePressTime presses
    onsAndOffs =
      sortOn (\(_, t, _) -> t)
      (zip3 xs times (repeat True) ++
      zip3 xs (map (+ window) times) (repeat False))

    xSets = scanl' addOrRemove (Set.empty, 0) onsAndOffs

    addOrRemove (currXs, _) (x, t, isOn) =
      ((if isOn then Set.insert else Set.delete) x currXs, t)

pitchMovingRange = movingRange notePressPitch
velocityMovingRange = movingRange notePressVelocity
