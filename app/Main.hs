
{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
module Main where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import MidiRhythm.Midi
import qualified Sound.MIDI.File as MIDI
import Sound.MIDI.File.Load as LoadMIDI
import MidiRhythm.NotePress
import MidiRhythm.Rhythm
import MidiRhythm.Search
import MidiRhythm.Histogram
import Data.List
import Data.Ord
import qualified Numeric.NonNegative.Wrapper as NonNeg
import Control.Arrow
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

instance ToJSON GroupCount
instance ToJSON NotePress
instance FromJSON NotePress

toIntJSON x = toJSON (fromIntegral x :: Int)
toFracJSON x = toJSON (NonNeg.toNumber x)

parseIntegral expected = withScientific expected $ pure . truncate

instance ToJSON (MovingRange Pitch)
instance ToJSON (MovingRange Velocity)

instance ToJSON Pitch where toJSON = toIntJSON
instance ToJSON Velocity where toJSON = toIntJSON
instance ToJSON ElapsedTime where toJSON = toIntJSON
instance ToJSON NonNeg.Double where toJSON = toFracJSON

instance FromJSON Pitch where parseJSON = parseIntegral "Pitch"
instance FromJSON Velocity where parseJSON = parseIntegral "Velocity"
instance FromJSON ElapsedTime where parseJSON = parseIntegral "ElapsedTime"

main :: IO ()
main = WS.runServer "0.0.0.0" 9160 application

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    let (Just presses) = decode msg :: Maybe [NotePress]
    print presses
    let conf = EmConfig (FitnessConfig (ChordDiffCoeffs 2 2 2 1) 10 4) (1 / 3)
    let hist = noteHistogram (NoteSimilarityCoeffs 1 0.5 0.5) 6000 256 presses
    let (GroupCount l r _) = maximumBy (comparing groupWeight)
              (filter (\g -> groupLeftBoundary g > 300) hist)
    let initStep = (l + r) `div` 2
    let pitchRanges = pitchMovingRange initStep presses
    let velocityRanges = velocityMovingRange initStep presses
    WS.sendTextData conn (encode $ object
      [ "histogram" .= hist
      , "pitchRanges" .= pitchRanges
      , "velocityRanges" .= velocityRanges ])
    let initBars = fixSize presses [initStep]
    _ <- runStateT (replicateM_ 50 (do
      (bars, fitness) <- emStep conf presses
      state <- get
      --lift $ print state
      let serialized = encode $ object ["bars" .= bars, "fitness" .= fitness]
      lift $ WS.sendTextData conn serialized
      )) (EmState initBars 0)
    return ()
