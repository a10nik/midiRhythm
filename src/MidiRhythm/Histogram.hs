{-# LANGUAGE DeriveGeneric #-}
module MidiRhythm.Histogram where

import MidiRhythm.NotePress
import MidiRhythm.Rhythm
import Data.List
import Data.Ord
import qualified Data.Map.Strict as Map
import qualified Numeric.NonNegative.Wrapper as NonNeg
import Numeric.NonNegative.Class
import Control.Arrow
import Control.Monad.State
import GHC.Generics

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

    distancesWithSignificance :: [(ElapsedTime, NonNeg.Double)]
    distancesWithSignificance =
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
      [(t `div` sliceLen, sgn) | (t, sgn) <- distancesWithSignificance]
