'use strict';

class Display extends Component {
  defaultInstalls() {
    return [Player, Board, Clock, Actions];
  }

  interestedTopics() {
    return ['got-table-id', 'got-table-info'];
  }

  parsedPlayers(json) {
    return json;
  }

  handleMessage(title, body) {
    if (title == 'got-table-id') {
      // console.log("Received table-id" + body.tableId);
      //begin getting table info on loop
    } else if (title == 'got-table-info') {
      this.msg('board-update', body.board);
      var pp = this.parsedPlayers(body.players);
      this.msg('players-update', pp);
      this.msg('status-update', body.status);
      // console.log("The status is" + JSON.stringify(body.status));
      // console.log(JSON.stringify(pp));
      //send status
      //send players
    }
  }

  init(o) {
    console.log(this.components);
    this.player = this.grab('Player');
    this.player.join();
    console.log(this.player);
  }

  // 0 not joined
  // 1 finding, 2 playing, 3 finished

  clicked(e) {
    let rect = window.client.canvases[0].getBoundingClientRect();
    let r  = {x: 1 + Math.floor((e.clientX - rect.left) / (rect.width / 5)), 
      y: 1 + Math.floor((e.clientY - rect.top) / (rect.height / 5))};
      console.log("r.x:" + r.x +"/r.y:"+r.y);
    window.client.display.makeMove(r);
  }

  mouseMoved(e) {
    let rect = window.client.canvases[0].getBoundingClientRect();
    var mx = e.clientX - rect.left;
    var my = e.clientY - rect.top;
    var wsize = rect.width / 5;
    var hsize = rect.height / 5;
    let r  = {x: 1 + Math.floor(mx / wsize), 
      y: 1 + Math.floor(my / hsize)};
      // console.log("Mouse move. r.x:" + r.x +"/r.y:"+r.y);
    var centerX = ((r.x - 1) * hsize) + (0.5 * wsize);
    var centerY = ((r.y - 1) * hsize) + (0.5 * hsize);
    var xDist = mx - centerX;
    var yDist = my - centerY;

    var wall = null;
    if (Math.abs(xDist) > Math.abs(yDist)) {
      if (xDist < -Math.abs(yDist)) {
        wall = "west";
      } else if (xDist > Math.abs(yDist)) {
        wall = "east";
      } else {
      }
    } else {
      if (yDist < -Math.abs(xDist)) {
        wall = "north";
      } else if (yDist > Math.abs(xDist)) {
        wall = "south";
      }  
    }

    this.wall = wall;
    this.wallR = r;
    
  }

  loopKeyboardInput(down, up, pressing, pressed) {
    //
  }


  makeMove(r) {
    console.log("Yes, sending make-move");
    this.msg('make-move', {x: r.x, y: r.y, wall: this.wall});
  }
  
}