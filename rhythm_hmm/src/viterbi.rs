use std::f32;
use noisy_float::prelude::*;

type Obs = usize;
type State = usize;

#[derive(Clone, Copy, Debug)]
struct ProbAndPrev {
    prob: f32,
    prev: Option<State>,
}

fn viterbi_i(obs: &Vec<Obs>,
             states: &Vec<State>,
             start_p: &Vec<f32>,
             trans_p: &Vec<Vec<f32>>,
             emit_p: &Vec<Vec<f32>>)
             -> (f32, Vec<State>) {

    let prevs = {
        let mut prevs_mut = vec![vec![]; states.len()];
        for (prev, currs) in trans_p.iter().enumerate() {
            for (curr, &prob) in currs.iter().enumerate() {
                if prob.is_finite() {
                    prevs_mut[curr].push(prev);
                }
            }
        }
        prevs_mut
    };

    let mut v: Vec<Vec<ProbAndPrev>> =
        vec![vec![ProbAndPrev { prob: f32::NEG_INFINITY, prev: None }; states.len()]; obs.len()];
    for st in states.iter() {
        v[0].insert(*st,
                    ProbAndPrev {
                        prob: start_p[*st] + emit_p[*st][obs[0]],
                        prev: None,
                    });
    }
    // Run Viterbi when t > 0
    for t in 1..obs.len() {
        for &st in states {
            let ref prevs_of_st = prevs[st];
            let (prev_st, prob) = prevs_of_st.iter()
                .zip(prevs_of_st.iter()
                    .map(|&prev_st| v[t - 1][prev_st].prob + trans_p[prev_st][st]))
                .max_by_key(|&(_, prob)| n32(prob))
                .unwrap();
            v[t].insert(st,
                        ProbAndPrev {
                            prob: prob + emit_p[st][obs[t]],
                            prev: Some(*prev_st),
                        });
        }
    }
    // The highest probability
    let (best_state, &ProbAndPrev { prob: best_prob, prev: best_prev }) = v.last()
        .unwrap()
        .iter()
        .enumerate()
        .max_by_key(|&(_, p)| n32(p.prob))
        .unwrap();
    let mut opt: Vec<State> = vec![best_state];
    let mut previous = best_prev;
    // Follow the backtrack till the first observation
    for t in (0..v.len() - 1).rev() {
        let ProbAndPrev { prev, .. } = v[t + 1][previous.unwrap()];
        opt.insert(0, prev.unwrap());
        previous = prev;
    }
    (best_prob, opt)
}

fn normalize(vec: Vec<f32>) -> Vec<f32> {
    let sum: f32 = vec.iter().sum();
    vec.iter().map(|x| (x / sum).log2()).collect()
}

pub fn viterbi<'a, Obs, State, StartP, TransP, EmissionP>(obs: &Vec<Obs>,
                                                          states: &'a Vec<State>,
                                                          start_p: StartP,
                                                          trans_p: TransP,
                                                          emission_p: EmissionP)
                                                          -> (f32, Vec<&'a State>)
    where StartP: Fn(&State) -> f32,
          TransP: Fn(&State, &State) -> f32,
          EmissionP: Fn(&State, &Obs) -> f32
{
    let ref obs_int: Vec<usize> = (0..obs.len()).collect();
    let ref states_int: Vec<usize> = (0..states.len()).collect();
    let ref start_p_int: Vec<f32> = normalize(states.iter().map(|s| start_p(s)).collect());
    let ref trans_p_int: Vec<Vec<f32>> = states.iter()
        .map(|st1| normalize(states.iter().map(|st2| trans_p(st1, st2)).collect()))
        .collect();
    let ref emission_p_int: Vec<Vec<f32>> = states.iter()
        .map(|st| normalize(obs.iter().map(|o| emission_p(st, o)).collect()))
        .collect();
    let (res_p, res_int) = viterbi_i(obs_int,
                                     states_int,
                                     start_p_int,
                                     trans_p_int,
                                     emission_p_int);
    (res_p.exp2(), res_int.iter().map(|&st_i| &states[st_i]).collect())
}

#[test]
fn viterbi_wiki_test() {
    use std::collections::HashMap;

    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    enum State {
        Healthy,
        Fever,
    }

    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    enum Obs {
        Normal,
        Cold,
        Dizzy,
    }

    let mut start_p = HashMap::new();
    start_p.insert(State::Healthy, 0.6);
    start_p.insert(State::Fever, 0.4);

    let mut transition_p = HashMap::new();
    transition_p.insert((State::Healthy, State::Healthy), 0.7);
    transition_p.insert((State::Healthy, State::Fever), 0.3);
    transition_p.insert((State::Fever, State::Healthy), 0.4);
    transition_p.insert((State::Fever, State::Fever), 0.6);

    let mut emission_p = HashMap::new();
    emission_p.insert((State::Healthy, Obs::Normal), 0.5);
    emission_p.insert((State::Healthy, Obs::Cold), 0.4);
    emission_p.insert((State::Healthy, Obs::Dizzy), 0.1);
    emission_p.insert((State::Fever, Obs::Normal), 0.1);
    emission_p.insert((State::Fever, Obs::Cold), 0.3);
    emission_p.insert((State::Fever, Obs::Dizzy), 0.6);

    let obs = vec![Obs::Normal, Obs::Cold, Obs::Dizzy];
    let states = vec![State::Healthy, State::Fever];
    let (prob, xs) = viterbi(&obs,
                             &states,
                             |s| start_p[s],
                             |&s1, &s2| transition_p[&(s1, s2)],
                             |&s, &o| emission_p[&(s, o)]);

    assert_eq!(xs, vec![&State::Healthy, &State::Healthy, &State::Fever]);
    assert!((prob - 0.01512).abs() < 0.001,
            "expected prob 0.01512, got {}",
            prob);
}