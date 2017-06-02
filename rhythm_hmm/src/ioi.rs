use std::f32;
use statrs::distribution::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Ioi3 {
    pub iois: [u32; 3],
}

impl Ioi3 {
    pub fn len(&self) -> u32 {
        self.iois[0] + self.iois[1] + self.iois[2]
    }
}


#[test]
fn dur_triplets_are_compared() {
    let t1 = Ioi3 { iois: [10, 20, 40] };
    let t2 = Ioi3 { iois: [10, 20, 40] };
    let t3 = Ioi3 { iois: [10, 21, 40] };
    assert_eq!(t1, t2);
    assert!(t1 != t3);
}

pub struct Ioi3Prob {
    pub tempo_tolerance: f32,
    pub tempo: f32, // avg time a hidden 'duration unit' takes
}

impl Ioi3Prob {
    fn mean_and_dev(&self, values: &[f32]) -> (f32, f32) {
        let mean = values.iter().sum::<f32>() / values.len() as f32;
        let dev = (values.iter().map(|t| (t - mean) * (t - mean)).sum::<f32>() /
                   (values.len() as f32 - 1.0))
            .sqrt();
        // println!("got dev: {:?}, mean: {:?}, vals: {:?}", dev, mean, values);
        (mean, dev)
    }

    pub fn observe(&self, hidden: &Ioi3, obs: &Ioi3) -> f32 {
        let tempo: Vec<f32> =
            obs.iois.iter().zip(hidden.iois.iter()).map(|(&o, &h)| o as f32 / h as f32).collect();
        let (tempo_mean, _) = self.mean_and_dev(tempo.as_slice());
        let tempo_distr = Normal::new(tempo_mean as f64,
                                      (tempo_mean * self.tempo_tolerance) as f64)
            .unwrap();
        let tempo_uniformness = tempo.iter().map(|&t| tempo_distr.pdf(t as f64)).product::<f64>();
        // println!("obs {:?} from {:?}: {:?}",
        //          hidden.iois,
        //          obs.iois,
        //          tempo_prob);
        (tempo_uniformness * tempo_distr.pdf(self.tempo as f64)) as f32
    }

    pub fn transition(&self, obs: &Ioi3, next: u32) -> f32 {
        if obs.iois.iter().all(|&ioi| ioi == next) {
            0.7
        } else {
            let div_count: usize = obs.iois
                .iter()
                .filter(|&ioi| next % ioi == 0 || ioi % next == 0)
                .count();
            let mut partial_eq_bonus = 0;
            if obs.iois[0] + obs.iois[1] == obs.iois[2] + next {
                partial_eq_bonus += 2;
            }
            if obs.iois[1] == obs.iois[2] + next {
                partial_eq_bonus += 2;
            }
            if obs.iois[1] + obs.iois[2] == next {
                partial_eq_bonus += 2;
            }
            0.5 * ((div_count + partial_eq_bonus) as f32 / 6.0)
        }
    }
}


#[test]
fn different_triplet_can_be_observed_with_prob_less_than_20() {
    let prob = Ioi3Prob {
        tempo_tolerance: 0.1,
        tempo: 6.0,
    };
    let triplet = Ioi3 { iois: [1, 2, 4] };
    let triplet2 = Ioi3 { iois: [2, 2, 2] };
    let eq_prob = prob.observe(&triplet, &triplet2);
    assert!(eq_prob < 0.2, "prob was {}, more than 0.2", eq_prob);
}

#[test]
fn transition_to_the_other_duration_yields_prob_less_20() {
    let prob = Ioi3Prob {
        tempo_tolerance: 0.1,
        tempo: 6.0,
    };
    let triplet = Ioi3 { iois: [1, 2, 4] };
    let eq_prob = prob.transition(&triplet, 10);
    assert!(eq_prob < 0.2, "prob was {}, more than 0.2", eq_prob);
}
