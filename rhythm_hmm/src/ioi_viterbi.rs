use std::cmp;
use ioi::*;
use viterbi;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct KeyPress {
    time: u32,
    pitch: u8,
    velocity: u8,
    duration: u32,
}

pub fn join_chords(keys: &Vec<KeyPress>, tolerance: u32) -> Vec<(u32, Vec<KeyPress>)> {
    let mut chords: Vec<(u32, Vec<KeyPress>)> = vec![];
    let mut i = 0;
    while i < keys.len() {
        let chord_end = keys[i].time + cmp::min(keys[i].duration, tolerance);
        let chord = keys[i..keys.len()]
            .iter()
            .take_while(|k| k.time < chord_end)
            .map(|k| *k)
            .collect::<Vec<_>>();
        let loudest_time = chord.iter()
            .max_by_key(|k| k.velocity)
            .unwrap()
            .time;
        i += chord.len();
        chords.push((loudest_time, chord));
    }
    chords
}

fn to_ioi3s(times: &Vec<u32>) -> Vec<Ioi3> {
    times.windows(3).map(|w| Ioi3 { iois: [w[0], w[1], w[2]] }).collect()
}

#[test]
fn joins_overlaps_into_chords() {
    let k = |time: u32| {
        KeyPress {
            time: time,
            duration: 100,
            velocity: 100,
            pitch: 100,
        }
    };
    let chords = vec![k(0), k(10), k(50)];
    let times: Vec<u32> = join_chords(&chords, 50).iter().map(|&(t, _)| t).collect();
    assert_eq!(times, vec![10, 50]);
}

fn get_trans_p(prob: &Ioi3Prob, prev: &Ioi3, next: &Ioi3) -> f32 {
    if prev.iois[1..2] == next.iois[0..1] {
        prob.transition(prev, next.iois[2])
    } else {
        0.0
    }
}

fn get_emiss_p(prob: &Ioi3Prob, state: &Ioi3, obs: &Ioi3) -> f32 {
    prob.observe(state, obs)
}

fn get_states() -> Vec<Ioi3> {
    let mut res = vec![];
    for d1 in 1..9 {
        for d2 in 1..9 {
            for d3 in 1..9 {
                res.push(Ioi3 { iois: [d1, d2, d3] });
            }
        }
    }
    res
}

pub fn most_probable_times(times: &Vec<u32>, prob: &Ioi3Prob) -> (f32, Vec<u32>) {
    let obs = to_ioi3s(times);
    let trans_p = |prev: &Ioi3, next: &Ioi3| get_trans_p(prob, prev, next);
    let start_p = |st: &Ioi3| get_emiss_p(prob, st, &obs[0]);
    let emission_p = |state: &Ioi3, obs: &Ioi3| get_emiss_p(prob, state, obs);
    let states = get_states();
    let (p, probable_iois) = viterbi::viterbi(&obs, &states, start_p, trans_p, emission_p);
    let mut res_times: Vec<u32> = probable_iois[0].iois.iter().map(|&x| x).collect();
    let rest = probable_iois[1..].iter().map(|ioi3| ioi3.iois[2]);
    res_times.extend(rest);
    (p, res_times)
}

#[test]
pub fn most_probable_times_for_uniform_presses_are_uniform() {
    let prob = Ioi3Prob {
        tempo_tolerance: 0.1,
        score_uniformness: 0.1,
    };
    let times: Vec<u32> = vec![5000; 10];
    let (_, adj_times) = most_probable_times(&times, &prob);
    assert_eq!(adj_times, vec![adj_times[0]; 10]);
}