module Main where

import MidiRhythm.Rhythm
import MidiRhythm.Midi
import MidiRhythm.NotePress

import Sound.MIDI.File
import Sound.MIDI.File.Load
import qualified Data.EventList.Relative.TimeBody as RelEvList
import Data.EventList.Absolute.TimeBody as AbsEvList
import Sound.MIDI.Message.Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import Sound.MIDI.File.Event as Event
import Data.Foldable as Foldable

main :: IO ()
main = do
  midi <- fromFile "samples/sammy.mid"
  Foldable.mapM_ (print . pressesToChunks (ElapsedTime 4) . toPresses) (getTracks midi)
