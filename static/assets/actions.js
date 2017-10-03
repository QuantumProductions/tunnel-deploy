"use strict";

class Actions extends Component {
  init(o) {
    this.canvasId = 'actions';
    this.current = 'x';
    this.xa = {current: 0, next: 0};
    this.oa = {current: 0, next: 0};
  }

  canWall(tile) {
    return this.current == tile.owner;
  }

  interestedTopics() {
    return ['status-update', 'players-update', 'draw'];
  }

  handleMessage(t, b) {
    if (t == 'status-update') {
      this.current = b.current_player;
    } else if (t == 'players-update') {
      this.xa = b.x.actions;
      this.oa = b.o.actions;
    } else if (t == 'draw') {
      this.paint(b.canvas, b.context);
    }
  }

  paint(canvas, ctt) {
    if (canvas.id != this.canvasId) { return; }
    var scale = canvas.width / 400;
    var pts = scale * 60;
    ctt.font = '' + pts +'pt Courier New';
    ctt.fillStyle = Color.x;
    let w = canvas.width;
    let xal = 0.08 * w;
    let maxWidth = 0.5 * w;
    let h = canvas.height;
    if (this.current == 'x') {
      let xnow = this.nowmoves(this.xa.current);
      ctt.fillText(xnow, xal, 0.2 * h, maxWidth * 1.06);    
    }
    
    let xnext = this.current == 'x' ? this.nextmoves(this.xa.next) : this.nextmoves(this.xa.current);
    ctt.fillText(xnext, xal, 0.4 * h, maxWidth * 1.5);  

    ctt.fillStyle = Color.o;

    if (this.current == 'o') {
      let onow = this.nowmoves(this.oa.current);
      ctt.fillText(onow, xal, 0.7 * h, maxWidth * 1.06);  
    }
    
    let onext = this.current == 'o' ? this.nextmoves(this.oa.next) : this.nextmoves(this.oa.current);
    ctt.fillText(onext, xal, 0.9 * h, maxWidth * 1.5);  
  }


  nowmoves(n) {
    return "" + n + " moves left";
  }

  nextmoves(n) {
    if (n > 4) {
      n = 4;
    }
    return "" + n + " moves next turn"
  }
}

