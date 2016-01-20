module Lib where

import Sound.MIDI.File
import qualified Data.EventList.Relative.TimeBody as RelEvList
import Data.EventList.Absolute.TimeBody as AbsEvList
import Sound.MIDI.Message.Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import Sound.MIDI.File.Event as Event
import Data.Foldable as Foldable


getVoiceEvents track =
    length $
    slice pitch $
    mapMaybe eventToMaybeNoteEvent $
        RelEvList.toAbsoluteEventList 0 track

data NoteEvent = NoteOff Pitch
               | NoteOn Pitch Velocity
               deriving Show

eventToMaybeNoteEvent :: Event.T -> Maybe NoteEvent
eventToMaybeNoteEvent ev = do
                (_, voiceEvent) <- maybeVoice ev
                voiceEventToMaybeNoteEvent voiceEvent

voiceEventToMaybeNoteEvent :: Voice.T -> Maybe NoteEvent
voiceEventToMaybeNoteEvent (Voice.NoteOn p v) = Just (NoteOn p v)
voiceEventToMaybeNoteEvent (Voice.NoteOff p v) = Just (NoteOff p)
voiceEventToMaybeNoteEvent _ = Nothing

pitch :: NoteEvent -> Pitch
pitch (NoteOn p _) = p
pitch (NoteOff p) = p

type Hit t = (t, Velocity)
type Press t = (t, t, Velocity)
type PressesState t = ([Press t], Maybe (Hit t))
type TimedNoteEvent t = (t, NoteEvent)

addOrSkip :: Num t => PressesState t -> TimedNoteEvent t -> PressesState t
addOrSkip state@(_, Just _) (t, NoteOn _ _) = state
addOrSkip state@(_, Nothing) (t, NoteOff _) = state
addOrSkip (presses, Just (prevT, vel)) (t, NoteOff _) = ((prevT, t - prevT, vel):presses, Nothing)
addOrSkip (presses, Nothing) (t, NoteOn _ vel) = (presses, Just (t, vel))

toPresses :: Num t => [TimedNoteEvent t] -> [Press t]
toPresses = fst . foldl addOrSkip ([], Nothing)

