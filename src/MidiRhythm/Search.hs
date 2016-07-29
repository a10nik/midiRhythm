module MidiRhythm.Search where

import MidiRhythm.NotePress
import MidiRhythm.Rhythm
import Data.List
import Data.Ord
import qualified Numeric.NonNegative.Wrapper as NonNeg
import Control.Arrow
import Control.Monad.State

fixSizeRev :: [ElapsedTime] -> ElapsedTime -> [ElapsedTime]
fixSizeRev bars@(lastBar : restBars) lastT
  | sum' bars < lastT = fixSizeRev (lastBar : bars) lastT
  | sum' restBars > lastT = fixSizeRev restBars lastT
  | otherwise = bars

fixSize :: [NotePress] -> [ElapsedTime] -> [ElapsedTime]
fixSize presses bars = reverse
  (fixSizeRev (reverse bars) (notePressTime $ last presses))

data EmState = EmState [ElapsedTime] Int deriving (Show, Eq)

data EmConfig = EmConfig FitnessConfig NonNeg.Double deriving (Show, Eq)

emStep' :: EmConfig -> [NotePress] -> EmState -> EmState
emStep' (EmConfig conf varCoeff) presses (EmState bars i) =
  EmState bestVariation ((i + 1) `mod` length bestVariation)
  where
    fitness someBars = getFitness conf someBars presses
    variations = map (fixSize presses) (variate varCoeff i bars)
    bestVariation = minimumBy (comparing fitness) variations

emStep :: Monad m => EmConfig -> [NotePress] -> StateT EmState m ([ElapsedTime], NonNeg.Double)
emStep conf@(EmConfig fitConf _) presses = do
  state <- get
  let newState@(EmState bars _) = emStep' conf presses state
  put newState
  return (bars, getFitness fitConf bars presses)

variations :: ElapsedTime -> NonNeg.Double -> [ElapsedTime -> ElapsedTime]
variations bar ratio = map (+) [1 .. varLen] ++ map (flip (-)) [1 .. varLen]
  where
    varLen :: ElapsedTime
    varLen = round $ fromIntegral bar * NonNeg.toNumber ratio

variateBar :: NonNeg.Double -> ElapsedTime -> [ElapsedTime]
variateBar ratio bar = map ($ bar) (variations bar ratio)

variate2Bars :: NonNeg.Double -> (ElapsedTime, ElapsedTime) -> [(ElapsedTime, ElapsedTime)]
variate2Bars ratio (b1, b2) = map ($ b1) vars `zip` map ($ b2) (reverse vars)
  where
    vars = variations (min b1 b2) ratio

variate :: NonNeg.Double -> Int -> [ElapsedTime] -> [[ElapsedTime]]
variate ratio 0 [bar] = map (: []) (variateBar ratio bar)
variate ratio 0 (first : second : bars) =
  map (: second : bars) (variateBar ratio first) ++
  map (\(var1, var2) -> var1 : var2 : bars) (variate2Bars ratio (first, second))
variate ratio i (first : bars) = map (first :) (variate ratio (i - 1) bars)
