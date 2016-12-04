use std::f32;
use std::collections::HashMap;
use std;
// #[derive(Debug)]
// pub struct KeyPress {
// time: u32,
// pitch: u8,
// velocity: u8,
// duration: u32,
// }

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct IoiTriplet {
    iois: [u32; 3],
}

impl IoiTriplet {
    pub fn len(&self) -> u32 {
        self.iois[0] + self.iois[1] + self.iois[2]
    }
}

#[test]
fn dur_triplets_are_compared() {
    let t1 = IoiTriplet { iois: [10, 20, 40] };
    let t2 = IoiTriplet { iois: [10, 20, 40] };
    let t3 = IoiTriplet { iois: [10, 21, 40] };
    assert_eq!(t1, t2);
    assert!(t1 != t3);
}

pub struct TripletProb {
    variance: f32,
}

impl TripletProb {
    fn prob_dens_fn(x: f32, mean: f32, variance: f32) -> f32 {
        let diff = 1.0 - 0.5 * ((x - mean).abs() / (2.0 * variance)).tanh();
        diff
    }

    fn normalize(triplet: &IoiTriplet) -> [f32; 3] {
        let len = triplet.len() as f32;
        let n = |v: u32| v as f32 / len;
        [n(triplet.iois[0]), n(triplet.iois[1]), n(triplet.iois[2])]
    }

    pub fn are_eq(&self, t1: &IoiTriplet, t2: &IoiTriplet) -> f32 {
        let norm1 = Self::normalize(t1);
        let norm2 = Self::normalize(t2);
        let g = |ioi1, ioi2| Self::prob_dens_fn(ioi1, ioi2, self.variance);
        (g(norm1[0], norm2[0]) * g(norm1[1], norm2[1]) * g(norm1[2], norm2[2]))
    }
}

#[test]
fn equal_triplets_are_equal_with_prob_100() {
    let prob = TripletProb { variance: 0.1 };
    let triplet = IoiTriplet { iois: [1, 2, 3] };
    assert_eq!(prob.are_eq(&triplet, &triplet), 1.0);
}

#[test]
fn different_triplets_are_equal_with_prob_less_than_50() {
    let prob = TripletProb { variance: 0.1 };
    let triplet = IoiTriplet { iois: [1, 2, 3] };
    let triplet2 = IoiTriplet { iois: [2, 2, 2] };
    let eq_prob = prob.are_eq(&triplet, &triplet2);
    assert!(eq_prob < 0.5, "prob was {}, more than 0.5", eq_prob);
}


pub fn viterbi<Obs, State>(obs: &Vec<Obs>,
                           states: &Vec<State>,
                           start_p: &HashMap<State, f32>,
                           trans_p: &HashMap<(State, State), f32>,
                           emit_p: &HashMap<(State, Obs), f32>)
                           -> Vec<State>
    where State: Eq + std::hash::Hash + Copy + Clone,
          Obs: Eq + std::hash::Hash + Copy + Clone
{
    let mut v: Vec<HashMap<State, (f32, Option<State>)>> =
        obs.iter().map(|_| HashMap::new()).collect();
    for st in states.iter() {
        v[0].insert(*st,
                    (start_p.get(st).unwrap() * emit_p.get(&(*st, obs[0])).unwrap(), None));
    }
    // Run Viterbi when t > 0
    for t in 1..obs.len() {
        for st in states {
            let max_tr_prob = states.iter()
                .map(|prev_st| {
                    let &(prev_prob, _) = v[t - 1].get(prev_st).unwrap();
                    prev_prob * trans_p.get(&(*prev_st, *st)).unwrap()
                })
                .fold(0. / 0., f32::max);
            for prev_st in states {
                let &(prev_prob, _) = v[t - 1].get(&prev_st).unwrap();
                if prev_prob * trans_p.get(&(*prev_st, *st)).unwrap() == max_tr_prob {
                    let max_prob = max_tr_prob * emit_p.get(&(*st, obs[t])).unwrap();
                    v[t].insert(*st, (max_prob, Some(*prev_st)));
                    break;
                }
            }
        }
    }
    let mut opt: Vec<State> = Vec::new();
    // The highest probability
    let max_prob = v.last().unwrap().values().map(|&(prob, _)| prob).fold(0. / 0., f32::max);
    let mut previous: Option<State> = None;
    // Get most probable state and its backtrack
    for (st, &(prob, _)) in v.last().unwrap().iter() {
        if prob == max_prob {
            opt.push(*st);
            previous = Some(*st);
            break;
        }
    }
    // Follow the backtrack till the first observation
    for t in (0..v.len() - 1).rev() {
        let &(_, prev) = v[t + 1].get(&previous.unwrap()).unwrap();
        opt.insert(0, prev.unwrap());
        previous = prev;
    }
    opt
}

#[test]
fn viterbi_wiki_test() {

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
    let xs = viterbi(&obs, &states, &start_p, &transition_p, &emission_p);
    assert_eq!(xs, vec![State::Healthy, State::Healthy, State::Fever]);

}