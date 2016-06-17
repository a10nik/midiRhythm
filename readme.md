Here I'll keep the diary of incoming ideas on MIDI-music rhythmic analysis.

##About Rhythm in MIDI-Music

The problem of rhythmic analysis of MIDI-music arises when it is recorded from a real human performance on a
MIDI-controller (like a digital piano, for instance). Note, that MIDI format itself is capable of expressing the rhythmic
structure by explicitly specifying tempo and changes in it, but it's only the case with computer-generated music. In a
real MIDI-instrument recording, there is only information about beginning, ending and velocity of each note.

The common problem is that such recordings imminently contain plenty of minor rhythmic distortions. They can be roughly
classified by their time-aspect into permanent and temporary. Permanent distortions affect a prolonged segment of composition
(several bars at least) while temporary deal with tempo inside a bar. Permanent distortions in their turn subdivide into gradual
and abrupt distortions.

##Goals

The long-run objective is to accurately detect rhythmic structure of a musical fragment that is to establish measures and
determine the values of notes and rests (i.e. 1, 1/2, 1/4, etc).

###Bar Line Placement

An obvious path to take is to place bar lines first and then divide the bars into equal parts to form a grid to which
the notes inside a bar are to be aligned.

In real music the meter can often be hard to determine.
For instance, meters multiple of each other like 2/4 and 4/4 differ only with positions of stressed notes and
therefore are difficult to tell apart considering minor deviations from standards that musicians tend to make.
Of course, in such ambiguous cases we should allow our algorithm to choose either of the possible options.

There are some informal rules that bars partition discovered by the algorithm should satisfy.
- The bars should be small enough not to include the same rhythmic (time & velocity) pattern several times.
- The corresponding notes in consecutive bars should be similar to each other in terms of duration, velocity and (usually) pitch.
  In particular, the bars should be big enough to capture the whole recurring pattern (or otherwise there will be a
  big distinction between those with a strong beat and a weak beat).
- Consecutive bars should usually have similar duration. Exceptional are the cases of deliberate
  rhythmic distortion like fermata or an abrupt tempo change.

###Average Bar Length

Estimation of the average bar length in a specific segment of a composition does not immediately leads us to a solution
of a stated problem. Nevertheless, it can have some useful implications as a starting point in tempo analysis. For example,
it can act as an additional hint for choosing the basic bar length (TODO example where it would help) as well as serving for
an a posteriori fitness quality evaluation.
