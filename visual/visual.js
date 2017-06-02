var width = window.innerWidth - 100,
    height = 300;
var sidePadding = 20;
var bottomPadding = 30;

function renderPressesAndBars(presses, bars, durations) {
  var pitchExt = d3.extent(presses, function(p) { return p.notePressPitch; });
  var y = d3.scaleLinear()
      .range([height, 0])
      .domain([pitchExt[0] - 1, pitchExt[1]]);
  var x = d3.scaleLinear()
      .range([0, width])
      .domain([0, d3.max(presses, function(p) {
        return p.notePressTime + p.notePressDuration;
      })]);
  var xAxis = d3.axisBottom(x);
  var chart = d3.select(".presses")
    .attr("width", width + sidePadding * 2)
    .attr("height", height + bottomPadding);
  chart.selectAll("*").remove();
  var mainG = chart.append("g")
    .attr("width", width)
    .attr("height", height)
    .attr("transform", "translate(" + sidePadding + ", 0)");
  var color = d3.scaleLinear()
    .range([200, 0])
    .domain(d3.extent(presses, function(p) { return p.notePressVelocity; }));
  mainG.selectAll("rect")
      .data(presses)
      .enter()
      .append("rect")
      .attr("y", function(p) { return y(p.notePressPitch); })
      .attr("height", function(p) { return y(0) - y(1); })
      .attr("x", function(p) { return x(p.notePressTime); })
      .attr("width", function(p) { return x(p.notePressDuration); })
      .attr("fill", function(p) { return d3.rgb(color(p.notePressVelocity), color(p.notePressVelocity), color(p.notePressVelocity)).toString(); });
  mainG.selectAll("line")
      .data(bars)
      .enter()
      .append("line")
      .attr("x1", function(b) { return x(b) })
      .attr("y1", 0)
      .attr("x2", function(b) { return x(b) })
      .attr("stroke", "black")
      .attr("y2", height);
  mainG.selectAll("text")
      .data(bars)
      .enter()
      .append("text")
      .attr("x", function(b) { return x(b) - 10; })
      .attr("y", height / 2)
      .text((b, i) => durations[i])
      .attr("font-family", "sans-serif")
      .attr("font-size", "10px")
      .attr("fill", "black");
  
  mainG.append("g")
    .attr("transform", "translate(0, " + (height + 5) + ")")
    .call(xAxis);
}

function renderBeatSpectrum(hist) {
  var y = d3.scaleLinear()
      .range([height, 0])
      .domain([0, d3.max(hist, function(d) { return d.groupWeight; })]);
  var x = d3.scaleLinear()
      .range([0, width])
      .domain([0, d3.max(hist, function(d) { return d.groupRightBoundary; })]);
  var xAxis = d3.axisBottom(x);
  var chart = d3.select(".periods")
      .attr("width", width + sidePadding * 2)
      .attr("height", height + bottomPadding);
  chart.selectAll("*").remove();
  var mainG = chart.append("g")
      .attr("width", width)
      .attr("height", height)
      .attr("transform", "translate(" + sidePadding + ", 0)");
  mainG.selectAll("rect")
      .data(hist)
      .enter()
      .append("rect")
      .attr("y", function(d) { return y(d.groupWeight); })
      .attr("height", function(d) { return height - y(d.groupWeight); })
      .attr("x", function(d) { return x(d.groupLeftBoundary); })
      .attr("width", function(d) { return x(d.groupRightBoundary) - x(d.groupLeftBoundary); });
  mainG.append("g")
    .attr("transform", "translate(0, " + (height + 5) + ")")
    .call(xAxis);
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
    let input = Array.from(midiAccess.inputs.values())[0];
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
    };
}

navigator.requestMIDIAccess()
  .then(onMidiAccess, () => console.error("omg, no MIDI connected"))

document.getElementsByClassName("send")[0].addEventListener("click", calculateAndRender);

var maxBeatMs = 5000;
var spectrumIntervals = 250;

function getBeatSpectrum(presses) {
  let intervalLength = maxBeatMs / spectrumIntervals;
  let spectrum = Array(spectrumIntervals).fill().map((_,i) => ({
    groupLeftBoundary: i * intervalLength,
    groupRightBoundary: (i + 1) * intervalLength,
    groupWeight: 0
  }));
  let getIntervalNum = x => Math.floor(x / intervalLength);
  forSpannedPresses(presses, (p1, p2) => {
    let dist = p2.notePressTime - p1.notePressTime;
    spectrum[getIntervalNum(dist)].groupWeight += 1 / (Math.abs(p2.notePressVelocity - p1.notePressVelocity) || 1);
  });
  return spectrum;
}

function forSpannedPresses(presses, action) {
  for (let i = 0; i < presses.length; i++) {
    let firstPress = presses[i];
    for (let j = i; j < presses.length && presses[j].notePressTime - firstPress.notePressTime < maxBeatMs; j++) {
      action(firstPress, presses[j], i, j);
    }
  }
}

var matrixSize = 1000;

function scalarProd(v1, v2) {
  let sum = 0;
  for (let i = 0; i < v1.length; i++) {
    sum += v1[i] * v2[i];
  }
  return sum;
}

function cos(feat1, feat2) {
  let prod = scalarProd(feat1, feat2);
  let feat1Norm = scalarProd(feat1, feat1);
  let feat2Norm = scalarProd(feat2, feat2);
  return prod / Math.sqrt(feat1Norm * feat2Norm);
}

function getSimilarityMatrix(presses) {
  let maxTime = presses[presses.length - 1].notePressTime + 1;
  let interval = maxTime / matrixSize;
  let matrix = Array(matrixSize).fill().map((_,i) => Array(matrixSize).fill(0));
  let getInd = x => Math.floor(x / interval);
  let features = p => (
    [
      p.notePressTime,
      p.notePressVelocity,
      p.notePressDuration,
      p.notePressPitch
    ]
  );
  forSpannedPresses(presses, (p1, p2) => {
    matrix[getInd(p1.notePressTime)][getInd(p2.notePressTime)] += Math.abs(cos(features(p1), features(p2)));
  });
  return matrix;
}

function renderSimilarityMatrix(matrix) {
  let canvas = document.getElementsByClassName("similarity")[0];
  canvas.height = matrixSize;
  canvas.width = matrixSize;
  let context = canvas.getContext("2d");
  let data = context.getImageData(0, 0, matrixSize, matrixSize);
  for (let i = 0; i < matrixSize; i++) {
    for (let j = 0; j < matrixSize; j++) {
      let weight = matrix[i][j];
      if (weight) {
        data.data[((matrixSize - i) * matrixSize + j) * 4 + 3] = 255 - weight * 200;
      }
    }
  }
  context.putImageData(data, 0, 0);
}


function calculateAndRender() {
  recordingState.presses.sort((p1, p2) => p1.notePressTime - p2.notePressTime);
  renderPressesAndBars(recordingState.presses, [],[]);
  var spectrum = getBeatSpectrum(recordingState.presses);
  renderBeatSpectrum(spectrum);
  var matrix = getSimilarityMatrix(recordingState.presses);
  renderSimilarityMatrix(matrix);
}

document.getElementsByClassName("send")[0].onclick = () => {
  recordingState.presses.sort((p1, p2) => p1.notePressTime - p2.notePressTime);
  var socket = new WebSocket("ws://127.0.0.1:3012");
  socket.onopen = () => {
    socket.send(JSON.stringify(recordingState.presses));
  };
  socket.onmessage = event => {
    let message = JSON.parse(event.data);
    renderPressesAndBars(recordingState.presses, message.chordTimes, message.durations);
    debugger;
  }
}