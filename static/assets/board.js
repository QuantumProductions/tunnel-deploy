"use strict";

class Board extends Component {
  init(o) {
    this.rows = Board.defaultRows();
    this.canvasId = 'board';
  }

  getTile(x, y) {
    return this.rows[y-1][x-1];
  }

  interestedTopics() {
    return ['click', 'board-update', 'draw'];
  }

  paint(canvas, context) {
    var w = canvas.width;
    var squaresize = w / 5;
    var tilesize = squaresize * 0.96;
    var sepsize = squaresize * 0.04;
    var halfsep = sepsize * 0.5;
    var size = this.rows.length;
    var x = 0;
    var y = 0;
    context.fillStyle = Color.bg;
    for (var i = 0; i < size + 1; i++) {
      context.beginPath();
      context.moveTo(0, y - halfsep);
      context.lineTo(canvas.width, y - halfsep);
      context.lineTo(canvas.width, y + halfsep);
      context.lineTo(0, y + halfsep);
      context.lineTo(0, y);
      context.fill();
      y += squaresize;
    }
    y = 0;
    context.fillStyle = Color.bg;
    for (var i = 0; i < size + 1; i++) {
      context.beginPath();
      context.moveTo(x - halfsep, 0);
      context.lineTo(x + halfsep, 0);
      context.lineTo(x + halfsep, canvas.height);
      context.lineTo(x - halfsep, canvas.height);
      context.lineTo(x - halfsep, 0);
      context.fill();
      x += squaresize;
    }

    for (var i = 0; i < size; i++) {
      var row = this.rows[i];
      if (!row) { return;}
      for (var j = 0; j < row.length; j++) {
        var tileData = row[j];
        this.drawTile(tileData, j, i, context, canvas);
      }
    }
  }

  drawTile(tileData, x, y, context, canvas) {
    var w = canvas.width;
    var squaresize = w / 5;
    var tilesize = squaresize * 0.96;
    var sepsize = squaresize * 0.04;
    var halfsep = sepsize * 0.5;

    context.beginPath();
    context.fillStyle = tileData.bgColor();

    var UL = [(x * squaresize) + halfsep, y * squaresize + halfsep];
    var UR = [((x + 1) * squaresize) - halfsep, (y * squaresize) + halfsep];
    var BR = [((x + 1) * squaresize) - halfsep, ((y + 1) * squaresize) - halfsep];
    var BL = [((x * squaresize) + halfsep), ((y + 1) * squaresize) - halfsep];

    context.moveTo(UL[0], UL[1]);
    context.lineTo(UR[0], UR[1]);
    context.lineTo(UR[0], UR[1]);
    context.lineTo(BR[0], BR[1]);
    context.lineTo(BL[0], BL[1]);
    context.lineTo(UL[0], UL[1]);
    context.fill();

    if (tileData.recent()) {
      var recentSize = squaresize * 0.10;
      context.beginPath();
      context.fillStyle = Color.recent;
      context.arc(UL[0] + 0.5 * squaresize - halfsep, UL[1] + 0.5 * squaresize - halfsep, recentSize, 0, 2 * Math.PI, false);
      context.fill();
    }


    if (tileData.ridge()) {
      var R1 = UL;
      var R2 = UR;
      var R3 = BR;
      var R4 = BL;

    var ridge = 0.1 * squaresize;
    var topLeft = [UL[0]+ ridge, UL[1] + ridge];
    var bottomLeft = [BL[0] + ridge, BL[1] - ridge];
    var topRight = [UR[0] - ridge, UR[1] + ridge];
    var bottomRight = [BR[0] - ridge, BR[1] - ridge];

    context.beginPath();
    context.strokeStyle = Color.ridge;
    context.lineWidth = 2;
    context.moveTo(topLeft[0], topLeft[1]);
    context.lineTo(bottomLeft[0], bottomLeft[1]);
    context.stroke();
    // context.beginPath();
    context.lineTo(bottomRight[0], bottomRight[1]);
    context.stroke();
    // context.beginPath();
    context.lineTo(topRight[0], topRight[1]);
    context.stroke();
    // context.beginPath();
    context.lineTo(topLeft[0], topLeft[1]);
    context.stroke();
    
   
    context.stroke();
    context.closePath();
    context.lineWidth = 1;
  }

    if (tileData.wall != "none") {
      var W1;
      var W2;
      var W3;
      var W4;
      var thickness = 0.1 * squaresize;
      if (tileData.wall == 'north') {
        W1 = UL;
        W2 = UR;
        W3 = [UL[0], UL[1] + thickness];
        W4 = [UR[0], UR[1] + thickness];
      } else if (tileData.wall == 'east') {
        W1 = UR;
        W2 = BR;
        W3 = [UR[0] - thickness, UR[1]];
        W4 = [BR[0] - thickness, BR[1]];
      } else if (tileData.wall == 'south') {
        W1 = BR;
        W2 = BL;
        W3 = [BR[0], BR[1] - thickness];
        W4 = [BL[0], BL[1] - thickness]
      } else if (tileData.wall == 'west') {
        W1 = BL;
        W2 = UL;
        W3 = [BL[0] + thickness, BL[1]];
        W4 = [UL[0] + thickness, UL[1]];
      }

      if (!W1) { return;}
      context.beginPath();
      context.fillStyle = Color.wall;
      context.moveTo(W1[0], W1[1]);
      context.lineTo(W2[0], W2[1]);
      context.lineTo(W4[0], W4[1]);
      context.lineTo(W3[0], W3[1]);
      context.lineTo(W1[0], W1[1]);
      context.fill();
    }



  }

  assignRows(json) {
    this.rows = [];
    for (var jrow of json) {
      var row = [];
      for (var jtile of jrow) {
        row.push(new Tile(jtile));
      }
      this.rows.push(row);
    }
  }

  handleMessage(t, b) {
    if (t == 'draw') {
      if (b.canvas.id != this.canvasId) { return; }
      this.paint(b.canvas, b.context);
    } else if (t == 'board-update') {
      this.assignRows(b);
    }
  }

  static defaultRows(n, rows) {
    var rows = [];
    for (var i = 0; i < 5; i++) {
      rows.push(Board.defaultRow());
    }
    return rows;
  }

  static defaultRow() {
    var row = [];
    for (var i = 0; i < 5; i++) {
      row.push(Board.defaultTile());
    }

    return row;
  }

  static defaultTile() {
    return new Tile({owner: null, status: "empty", wall: null});
  }
}