var width = window.innerWidth - 100,
    height = 300;
var sidePadding = 20;
var bottomPadding = 30;

function renderPressesAndBars(presses, bars) {
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
  var cumBars = [];
  bars.reduce(function(a, b, i) { return cumBars[i] = a + b; }, 0);
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
      .data(cumBars)
      .enter()
      .append("line")
      .attr("x1", function(b) { return x(b) })
      .attr("y1", 0)
      .attr("x2", function(b) { return x(b) })
      .attr("stroke", "black")
      .attr("y2", height);
  mainG.append("g")
    .attr("transform", "translate(0, " + (height + 5) + ")")
    .call(xAxis);
}

function renderDiffs(hist) {
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

function renderRanges(ranges, svgClass) {
  var y = d3.scaleLinear()
      .range([height, 0])
      .domain([
        d3.min(ranges, function(r) { return r.movingRangeMin; }),
        d3.max(ranges, function(r) { return r.movingRangeMax; })
      ]);
  var x = d3.scaleLinear()
      .range([0, width])
      .domain([0, d3.max(ranges, function(r) { return r.movingRangeTime; })]);
  var xAxis = d3.axisBottom(x);
  var chart = d3.select(svgClass)
      .attr("width", width + sidePadding * 2)
      .attr("height", height + bottomPadding);
  chart.selectAll("*").remove();
  var mainG = chart.append("g")
      .attr("width", width)
      .attr("height", height)
      .attr("transform", "translate(" + sidePadding + ", 0)");
  var area = d3.area()
    .x(function(d) { return x(d.movingRangeTime); })
    .y0(function(d) { return y(d.movingRangeMax); })
    .y1(function(d) { return y(d.movingRangeMin); });
  mainG.selectAll("path")
      .data([ranges])
      .enter()
      .append("path")
      .attr("d", area);
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
    if (message.bars) {
      renderPressesAndBars(recordingState.presses, message.bars);
    }
    if (message.histogram) {
      renderDiffs(message.histogram);
    }
    if (message.pitchRanges) {
      renderRanges(message.pitchRanges, ".pitch");
    }
    if (message.velocityRanges) {
      renderRanges(message.velocityRanges, ".velocity");
    }
  };
}

renderDiffs([]);
renderPressesAndBars([], []);
