module Lib
    ( openSammy
    ) where

import Sound.MIDI.File
import Sound.MIDI.File.Load

openSammy :: IO T
openSammy = fromFile "samples/sammy.mid"
