'use strict';

class Clock extends Component {
  init(o) {
    this.canvasId = 'actions';
    this.otime = 0;
    this.xtime = 0;
  }

  interestedTopics() {
    return ['status-update', 'players-update', 'draw'];
  }

  handleMessage(t, b) {
    if (t == 'status-update') {
      if (b.current_player != this.currentPlayer) {
        //swap clocks, assign current player
      }
    } else if (t == 'players-update') {
      this.xtime = b.x.clock;
      this.otime = b.o.clock;
    } else if (t == 'draw') {
      this.paint(b.canvas, b.context);
    }
  }

  paint(canvas, ctt) {
    if (canvas.id != this.canvasId) { return; }
    var scale = canvas.width / 400;
    var pts = scale * 100;
    ctt.font = '' + pts +'pt Courier New';
    ctt.fillStyle = Color.x;
    ctt.textAlign = 'left';
    let xtime = this.time(this.xtime);
    ctt.fillText(xtime, 0.05 * canvas.width, 0.1 * canvas.height - 2, 0.5 * canvas.width);
    ctt.fillStyle = Color.o;
    let otime = this.time(this.otime);
    ctt.fillText(otime, 0.05 * canvas.width, 0.6 * canvas.height - 2, 0.5 * canvas.width);
  }

  time(t) {
    let min = Math.floor(t / 6000);
    let seconds = Math.floor(t / 100);
    var sec = seconds % 60;
    if (sec == 0) {
      sec = "00";
    } else if (sec < 10) {
      sec = "0" + sec;
    }
    if (isNaN(min) || isNaN(sec)) {
      return "";
    }
    return "" + min + ":" + sec;
  }


}