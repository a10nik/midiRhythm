{-# LANGUAGE ScopedTypeVariables #-}
module MidiRhythm.Rhythm where

import Control.Arrow
import MidiRhythm.NotePress
import qualified Data.Vector as Vec
import qualified Numeric.NonNegative.Wrapper as NonNeg

import Data.List
import Data.Maybe
import Data.Ratio


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

data BarSide = LeftBar | RightBar
  deriving (Eq)

data SuperimposedInfo t = SuperimposedInfo {
  barSide :: BarSide,
  barScaledTime :: t
}

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

getSuperimposedChords = getSuperimposedChunks notePressTime
sum' :: (Num t) => [t] -> t
sum' = foldl' (+) 0

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

type AvgPress = (NonNeg.Double, NonNeg.Double, NonNeg.Double, NonNeg.Double)

avgPress :: [NotePress] -> Maybe AvgPress
avgPress [] = Nothing
avgPress presses =  Just ( avg notePressVelocity
                    , avg notePressDuration
                    , avg notePressPitch
                    , NonNeg.fromNumber (genericLength presses))
  where
    avg getX = NonNeg.fromNumber ((average $ map getX presses) :: Double)

data ChordDiffCoeffs = ChordDiffCoeffs {
  velocityCoeff :: NonNeg.Double,
  pitchCoeff :: NonNeg.Double,
  durationCoeff :: NonNeg.Double,
  countCoeff :: NonNeg.Double
} deriving (Show, Eq)

avgPressDiff :: ChordDiffCoeffs -> AvgPress -> AvgPress -> NonNeg.Double
avgPressDiff (ChordDiffCoeffs velC ptC durC ctC)
            (vel1, dur1, pt1, ct1)
            (vel2, dur2, pt2, ct2) =
                velC * nonNegDiff vel1 vel2 +
                ptC * nonNegDiff pt1 pt2 +
                durC * nonNegDiff dur1 dur2 +
                ctC * nonNegDiff ct1 ct2
  where
    sqr x = x * x
    nonNegDiff x y = NonNeg.fromNumber $
      sqr ((NonNeg.toNumber x :: Double) - (NonNeg.toNumber y :: Double))

data FitnessConfig = FitnessConfig {
  chordDiffConfig :: ChordDiffCoeffs,
  granularity :: NonNeg.Integer,
  barDiffPenalty :: NonNeg.Double
} deriving (Show, Eq)

getFitness :: FitnessConfig -> [ElapsedTime] -> [NotePress] -> NonNeg.Double
getFitness
  (FitnessConfig (ChordDiffCoeffs velCf pitCf durCf ctCf) gran barPenalty)
  barLengths presses =
      sum' (map barNormDiff barTimes) + avgBarLenDiff * barPenalty
    where
      barStarts = scanl' (+) 0 barLengths
      nextBarLengths = tail barLengths
      barTimes = zip3 barStarts barLengths nextBarLengths

      lenDiff a b = 1 - min a b % max a b
      avgBarLenDiff :: NonNeg.Double
      avgBarLenDiff = realToFrac
        (average $ map (\(_, f, s) -> lenDiff f s) barTimes)

      barNormDiff (start, first, next) =
        sum' $
            map (chordNormDiff velRange pitchRange durRange ctRange) avgPresses
          where
            chunks = getSuperimposedChords start first next gran presses
            avgPresses = map (avgPress *** avgPress) chunks
            (avgVels, avgPits, avgDurs, avgCounts) =
              unzip4 (catMaybes (uncurry mappend (unzip avgPresses)))
            range xs = max (maximum xs - minimum xs) 1
            velRange = range avgVels
            pitchRange = range avgPits
            durRange = range avgDurs
            ctRange = range avgCounts

      chordNormDiff _ _ _ _ (Nothing, Nothing) = 0
      chordNormDiff _ _ _ _ (Nothing, _) = velCf + pitCf + durCf + ctCf
      chordNormDiff _ _ _ _ (_, Nothing) = velCf + pitCf + durCf + ctCf
      chordNormDiff velR pitR durR ctR (Just ps1, Just ps2) =
          avgPressDiff conf ps1 ps2
        where
          conf = ChordDiffCoeffs
            (velCf / velR)
            (pitCf / pitR)
            (durCf / durR)
            (ctCf / ctR)
