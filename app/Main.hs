module Main where

import Lib
import Sound.MIDI.File
import qualified Data.EventList.Relative.TimeBody as RelEvList
import qualified Data.EventList.Absolute.TimeBody as AbsEvList
import Sound.MIDI.File.Event
import Data.Foldable

flatMap :: Data.Foldable.Foldable t => (a -> t b) -> [a] -> [b]
flatMap f = concatMap (Data.Foldable.toList . f)

main :: IO ()
main = do 
  midi <- openSammy
  mapM_ (print
                    . flatMap (\(t, ev) -> maybeVoice ev)
                    . AbsEvList.toPairList
                    . RelEvList.toAbsoluteEventList 0
                    ) (getTracks midi)
