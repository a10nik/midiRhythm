use std::f32;

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
    pub score_uniformness: f32,
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
        let (_, tempo_dev) = self.mean_and_dev(tempo.as_slice());
        let tempo_prob = if tempo_dev < self.tempo_tolerance {
            1.0
        } else {
            self.tempo_tolerance / tempo_dev
        };
        // println!("obs tempo dev: {:?}", tempo_dev);
        tempo_prob
    }

    pub fn transition(&self, obs: &Ioi3, next: u32) -> f32 {
        let (_, dev) = self.mean_and_dev(&[obs.iois[0] as f32,
                                           obs.iois[1] as f32,
                                           obs.iois[2] as f32,
                                           next as f32]);
        let max_dev = if dev < self.score_uniformness {
            1.0
        } else {
            self.score_uniformness / dev
        };
        max_dev
    }
}


#[test]
fn different_triplet_can_be_observed_with_prob_less_than_20() {
    let prob = Ioi3Prob {
        tempo_tolerance: 0.1,
        score_uniformness: 0.1,
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
        score_uniformness: 0.1,
    };
    let triplet = Ioi3 { iois: [1, 2, 4] };
    let eq_prob = prob.transition(&triplet, 10);
    assert!(eq_prob < 0.2, "prob was {}, more than 0.2", eq_prob);
}
