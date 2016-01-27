Here I'll keep the diary of incoming ideas on MIDI-music rhythmic analysis.

##About rhythm MIDI-music

The problem of rhythmic analysis of MIDI-music arises when it is recorded from a real human performance on a
MIDI-controller (like a digital piano, for instance). Note, that MIDI format itself is capable of expressing the rhythmic
structure by explicitly specifying tempo and changes in it, but it's only the case with computer-generated music. In a
real MIDI-instrument recording, there is only information about beginning, ending and velocity of each note.

##Goals

The long-run objective is to accurately detect rhythmic structure of a musical fragment that is to establish measures and determine
notes and rest values (i.e. 1, 1/2, 1/4, etc.).

The first step is to place bar lines appropriately. Usually that means contents of nearby bars being somewhat similar to each other.
