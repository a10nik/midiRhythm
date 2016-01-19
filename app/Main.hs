module Main where

import Lib
import Sound.MIDI.File
import qualified Data.EventList.Relative.TimeBody as EventList

printTrackInfo pairs = do
  print $ map (\(t, e) -> e) pairs

main :: IO ()
main = do 
  midi <- openSammy
  mapM_ (printTrackInfo . EventList.toPairList) (getTracks midi)
