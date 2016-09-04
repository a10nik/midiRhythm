
let strokeWidth = 10;
let pitchOffset = 5;
let timeFactor = 1/20;

function renderPressesAndBars(presses, bars) {
	let canvas = document.getElementsByClassName("presses")[0];
	let ctx = canvas.getContext("2d");
	ctx.clearRect(0, 0, canvas.width, canvas.height);

	let lowestVelocity = Math.min.apply(null, presses.map(p => p.notePressVelocity));
	let highestVelocity = Math.max.apply(null, presses.map(p => p.notePressVelocity));
	let lowestPitch = Math.min.apply(null, presses.map(p => p.notePressPitch));
	let highestPitch = Math.max.apply(null, presses.map(p => p.notePressPitch));
	canvas.width = timeFactor * (presses[presses.length - 1].notePressTime + presses[presses.length - 1].notePressDuration);
	canvas.height = strokeWidth + pitchOffset * (highestPitch - lowestPitch);
	presses.forEach(p => {
		let v = (p.notePressVelocity - lowestVelocity) / (highestVelocity - lowestVelocity + 1) * 255;
		let r = Math.round(v);
		let g = Math.round(255 - v);
		let b = Math.round(255 - v);
		ctx.fillStyle = `rgb(${r}, ${g}, ${b})`;
		ctx.fillRect(timeFactor * p.notePressTime, (highestPitch - p.notePressPitch) * pitchOffset, timeFactor * p.notePressDuration, strokeWidth);
	});
	let prev = 0;
	bars.forEach(b => {
		ctx.fillStyle = "black";
		ctx.fillRect(timeFactor * (prev + b), 0, 1, canvas.height);
		prev += b;
	});
}

function renderDiffs(hist) {
	let canvas = document.getElementsByClassName("hist")[0];
	let ctx = canvas.getContext("2d");
	ctx.clearRect(0, 0, canvas.width, canvas.height);
	let highestWeight = Math.max.apply(null, hist.map(g => g.groupWeight));
	canvas.width = 1500;
	canvas.height = 400;
	let timeFactor = canvas.width / hist[hist.length - 1].groupRightBoundary;
	hist.forEach(g => {
		ctx.fillStyle = '#888888';
		let height = canvas.height * g.groupWeight / highestWeight;
		ctx.fillRect(timeFactor * g.groupLeftBoundary, canvas.height - height, timeFactor * (g.groupRightBoundary - g.groupLeftBoundary), height);
	});
}


let recordingState = {
	presses: [],
	currentlyPressed: [],
	firstNoteTime: -1
};

function onNoteOn(time, velocity, pitch) {
	if (recordingState.firstNoteTime === -1)
		recordingState.firstNoteTime = time;
	recordingState.currentlyPressed[pitch] = { time: time, velocity: velocity };
}

function onNoteOff(time, pitch) {
	let noteOn = recordingState.currentlyPressed[pitch];
	recordingState.presses.push({ 
		notePressTime: noteOn.time - recordingState.firstNoteTime,
		notePressVelocity: noteOn.velocity,
		notePressDuration: time - noteOn.time,
		notePressPitch: pitch
	});
	renderPressesAndBars(recordingState.presses, []);
}


function onMidiAccess(midiAccess) {
  	console.log("MIDI ready!");
  	let input = midiAccess.inputs.get(0);
	input.onmidimessage = event => {
		if (event.data.length === 3) {
			let pitch = event.data[1];
			let velocity = event.data[2];
			let time = Math.round(event.timeStamp);
			if (event.data[0] === 0x90 && velocity > 0)
				onNoteOn(time, velocity, pitch);
			else if (event.data[0] === 0x90 && velocity === 0)
				onNoteOff(time, pitch)
			else if (event.data[0] === 0x80)
				onNoteOff(time, pitch)
		}
	}
}

navigator.requestMIDIAccess()
	.then(onMidiAccess, () => console.error("omg, no MIDI connected"))

document.getElementsByClassName("send")[0].onclick = () => {
	var socket = new WebSocket("ws://localhost:9160");
	socket.onopen = () => {
		socket.send(JSON.stringify(recordingState.presses.sort((p1, p2) => p1.notePressTime - p2.notePressTime)));		
	};
	socket.onmessage = event => {
		let message = JSON.parse(event.data);
		if (message.bars)
			renderPressesAndBars(recordingState.presses, message.bars);
		if (message.histogram)
			renderDiffs(message.histogram)
	};
}