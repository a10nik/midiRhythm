extern crate serde_json;
extern crate ws;
extern crate rhythm_hmm;
use rhythm_hmm::*;
use rhythm_hmm::ioi::*;
fn main() {
    let prob = Ioi3Prob { tempo_tolerance: 0.1 };
    let observed_durations: Vec<u32> = vec![177, 175, 193, 167, 166, 178, 189, 380, 357, 78, 116,
                                            217, 386, 168, 165, 185, 191, 165, 206, 162, 198, 397,
                                            364, 67, 108, 209, 365, 614];
    let intended_durations = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
    let (_, res) =
        ioi_viterbi::most_probable_times(&observed_durations, &intended_durations, &prob);
    println!("res {:?}", res);
}
