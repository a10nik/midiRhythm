module MidiRhythm.Midi (
    toPresses,
) where

import MidiRhythm.NotePress

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import qualified Sound.MIDI.File.Event as Event

import Data.Foldable as Foldable
import Control.Arrow

import qualified Data.EventList.Relative.TimeBody as RelEvList
import Data.EventList.Absolute.TimeBody
import GHC.Exts

import qualified Numeric.NonNegative.Wrapper as NonNeg

data NoteEvent = NoteOff Pitch
               | NoteOn Pitch Velocity
               deriving Show

pitch :: NoteEvent -> Pitch
pitch (NoteOn p _) = p
pitch (NoteOff p) = p



eventToMaybeNoteEvent :: Event.T -> Maybe NoteEvent
eventToMaybeNoteEvent ev = do
                (_, voiceEvent) <- Event.maybeVoice ev
                voiceEventToMaybeNoteEvent voiceEvent

voiceEventToMaybeNoteEvent :: Voice.T -> Maybe NoteEvent
voiceEventToMaybeNoteEvent (Voice.NoteOn p v) = Just (NoteOn (ourPitch p) (ourVelocity v))
voiceEventToMaybeNoteEvent (Voice.NoteOff p v) = Just (NoteOff (ourPitch p))
voiceEventToMaybeNoteEvent _ = Nothing

ourPitch = Pitch . NonNeg.fromNumberUnsafe . Voice.fromPitch
ourVelocity = Velocity . NonNeg.fromNumberUnsafe . Voice.fromVelocity

type Hit = (ElapsedTime, Velocity)

type PressesState = ([Press], Maybe Hit)

type TimedNoteEvent = (ElapsedTime, NoteEvent)

addPressOrSkip :: PressesState -> TimedNoteEvent -> PressesState
addPressOrSkip state@(_, Just _) (t, NoteOn _ _) = state
addPressOrSkip state@(_, Nothing) (t, NoteOff _) = state
addPressOrSkip (presses, Just (prevT, vel)) (t, NoteOff _) =
    (Press prevT vel (t - prevT) : presses, Nothing)
addPressOrSkip (presses, Nothing) (t, NoteOn _ vel) = (presses, Just (t, vel))

noteEventsToPresses :: [TimedNoteEvent] -> [Press]
noteEventsToPresses = fst . foldl addPressOrSkip ([], Nothing)


sliceByPitches = slice pitch .
                     mapMaybe eventToMaybeNoteEvent .
                     RelEvList.toAbsoluteEventList 0

pitchAndPressesToNotePresses (pitch, presses) =
    map (\(Press t vel dur) -> NotePress t vel dur pitch) presses


toPresses :: MidiFile.Track -> [NotePress]
toPresses =
    sortWith getTime
    . foldMap (pitchAndPressesToNotePresses . second (
        noteEventsToPresses
        . map (first ElapsedTime)
        . toPairList))
    . sliceByPitches
