extern crate serde_json;
extern crate ws;
extern crate rhythm_hmm;
use rhythm_hmm::*;

fn main() {
    let prob = ioi::Ioi3Prob {
        tempo_tolerance: 40.0,
        score_uniformness: 150.0,
    };
    let times: Vec<u32> = vec![212, 232, 213, 223, 217, 221, 211, 404, 433, 115, 144, 250, 450,
                               210];
    let (p, res) = ioi_viterbi::most_probable_times(&times, &((1..16).collect()), &prob);
    println!("p={}, res={:?}", p, res);
}
