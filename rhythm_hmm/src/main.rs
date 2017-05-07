#[macro_use]
extern crate serde_json;
extern crate ws;
extern crate rhythm_hmm;
use rhythm_hmm::*;
use rhythm_hmm::ioi::*;


fn main2() {
    let prob = Ioi3Prob { tempo_tolerance: 0.1 };
    let observed_durations: Vec<u32> = vec![177, 175, 193, 167, 166, 178, 189, 380, 357, 78, 116,
                                            217, 386, 168, 165, 185, 191, 165, 206, 162, 198, 397,
                                            364, 67, 108, 209, 365, 614];
    let intended_durations = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
    let (_, res) =
        ioi_viterbi::most_probable_times(&observed_durations, &intended_durations, &prob);
    println!("res {:?}", res);
}


fn main() {
    // Listen on an address and call the closure for each connection
    if let Err(error) = ws::listen("127.0.0.1:3012", |out| {

        // The handler needs to take ownership of out, so we use move
        move |msg: ws::Message| {
            let msg_text = msg.into_text().unwrap();
            println!("Server got message '{}'. ", msg_text);
            let presses_json = serde_json::from_str::<serde_json::Value>(&msg_text).unwrap();
            let presses = presses_json.as_array()
                .unwrap()
                .iter()
                .map(|press_val| press_val.as_object().unwrap())
                .map(|press_json| {
                    ioi_viterbi::KeyPress {
                        time: press_json["notePressTime"].as_u64().unwrap() as u32,
                        pitch: press_json["notePressPitch"].as_u64().unwrap() as u8,
                        velocity: press_json["notePressVelocity"].as_u64().unwrap() as u8,
                        duration: press_json["notePressDuration"].as_u64().unwrap() as u32,
                    }
                })
                .collect::<Vec<_>>();
            // Handle messages received on this connection
            println!("Received {} presses", presses.len());
            let grouped = ioi_viterbi::join_chords(&presses, 150)
                .iter()
                .map(|&(t, _)| t)
                .collect::<Vec<_>>();
            let durs = (1..grouped.len()).map(|i| grouped[i] - grouped[i - 1]).collect::<Vec<_>>();
            let prob = Ioi3Prob { tempo_tolerance: 0.1 };
            let possible_durations = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
            let (_, res) = ioi_viterbi::most_probable_times(&durs, &possible_durations, &prob);
            println!("recognized {:?}", res);
            // Use the out channel to send messages back
            let res_json = json!({
                "durations": res,
                "chordTimes": grouped
            });
            out.send(res_json.to_string())
        }

    }) {
        // Inform the user of failure
        println!("Failed to create WebSocket due to {:?}", error);
    }

}
