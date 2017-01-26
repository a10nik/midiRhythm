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

###Bar Lines and Fitness Maximization

Such heuristic tasks as ours can usually be formulated in terms of a fitness function maximization problem.
That involves divining a real-valued function that given a solution indicates how fitting it is.
In our case the fitness function is suggested to be defined as follows: we slice each bar equally
into N relatively small time segments each one to be treated as a set of simultaneous key presses. Then we calculate difference of the
corresponding segments in all the pairs of adjacent bars, difference of segments being defined as a weighted Cartesian distance
between the means of the two press sets, each represented by a vector of mean velocity, pitch, and duration.
To discourage choosing completely different bar lengths we should also subtract a difference between each consecutive bar lengths
with some coefficient denoting tolerance to tempo changes.

###Maximization

Once the fitness function is defined, the next objective is to find out where its maximum is reached.
Common calculus methods of maximization such as binsearch or gradient descent usually impose requirements on the function
like differentiability or convexity that our case most definitely fails to satisfy. The other approach is to give the stochastic
optimization methods a try. As a rule they trade off the guarantee of solution correctness for the lack of assumptions on the function regularity.
There is a well-studied family of random-search methods that operate on such conditions. Additionally, in the early 2000-s genetic algorithms have
rapidly grown on popularity as a solution search method.

###Genetic Approach

If we are to determine the bar lengths without any a-priori information but the key presses themselves, a genetic algorithm is a decent way to go.
To define a genetic algorithm we should choose the encoding for the solutions to evolve as well as the operations through which they are to be modified:
the mutation and crossover operations. Intuitively, when two variants of bar placement are to be crossed over, they should produce an offspring that
before some point in time coheres with one parent, and after that point with another. Normally we would have to deal with a tempo irregularity
at the point of crossover, but instead we can make use of the fact that fitness would not change drastically were we to shift the bar lines half a measure
back or forth. That's why to perform a crossover we will simply choose the bar lengths from one parent up to some point, and then of another.

As one can see, our definition of crossover largely matches that of a standard genetic algorithm. But that should not be the case with mutation. Originally,
mutation implies there will be significant local changes in the solution and in our case such choice would backfire more often than not. Instead we define
the mutation like that: after the mutation a randomly chosen segment of music will be cut evenly into a random amount of bars.

TODO

###Periodogram as an Initial Estimation

Whatever results are achievable by a genetic approach, it lacks a significant advantage of more crude methods: predictability of their results.
For instance a sort of enchanced periodogram could prove a useful starting point for somehow less intelligent searching methods like a coordinate descent or
random search. For reference, by a simple periodogram here we mean the distribution of pairwise time differences between presses.
The proposed way of enchancing it will largely stem from relying on musical specifics as well as putting some another available non-temporary data to use. It is that instead of equally influencing the distribution, each pair of presses will have its contribution proportional to
the similarity of the two notes in pitch, duration and velocity. When choosing the coefficients to calculate such similarity one should take into account
the fact difference in velocity is much more distinctive to a human ear than that in either pitch or duration. In our research we used the coefficients of
1, .5 and .5 for velocity, pitch and duration respectively.

###Bayessian methods

In 2000-s the University of Tokyo published a series of [papers](http://ismir2003.ismir.net/papers/Takeda.pdf) on MIDI music transcription using hidden Markov models. They make the assumption that a note score duration (as specified in a score) is largely determined by the preceding three score durations. In other words, once you encounter a couple of quarter notes that is much more likely that the next one is also a quarter, than for instance a 7/16. Obviously it is just as well determined by the performed duration of a note, since naturally two consequent notes performed in 108 ms and 386 ms are very unlikely to be a pair of quarters but rather a quarter and a whole or a quarter and a dotted half.

Thus, a Markov model can be formulated as follows: the states are trigrams of distances between notes (or inter-onset intervals, IOIs, according to the Japanese papers) as defined in score, there being k (about 20) possible values for each, k^3 states in total. Observations are the trigrams of performed IOIs. From each state s<sub>i</sub> = (A, B, C) there are k reachable states s<sub>j</sub> = (B, C, x), and the music of n notes is represented as n-2 transitions between states. Two kinds of probability are considered given (estimated based on samples): the transition probability for all pairs of states and the emission probability - the chance to obtain an observation from a state.

For the sake of illustration: the transition probability between (1/4, 1/4, 1/2) and (1/4, 1/2, <b>1/4</b>) defines the likelihood of a <b>1/4</b> following the rhythmic pattern 1/4, 1/4, 1/2. And the emission probability of (103 ms, 86 ms, 169 ms) from state (1/4, 1/8, 3/16) stands for the probability that 1/4, 1/8, 3/16 in score are performed as 103 ms, 86 ms, 169 ms.

Given the observations o, the chance of transition from state s<sub>1</sub> to s<sub>2</sub>:

    transition_given_the_observations(s_1, s_2, o) = transition(s_1, s_2) * emission(s_2, o_2)

The goal is to find the most probable states (score durations) provided the observations (performed durations). Such type of problems has an efficient solution - [the Viterbi algorithm](https://en.wikipedia.org/wiki/Viterbi_algorithm).
