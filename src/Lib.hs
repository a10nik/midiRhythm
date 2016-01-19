module Lib
    ( openSammy
    ) where

import Sound.MIDI.File.Load

openSammy :: IO ()
openSammy = do
        midi <- fromFile "samples/sammy.mid"
        print midi