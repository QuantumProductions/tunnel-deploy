"use strict";

class Client {
  styleCanvas(canvas) {
  }

  generateCanvas() {
    
  }

  installRendering() {
    Color.setup();
    this.display = new Display();
    this.installCanvases();
  }

  existingCanvases() {
    return [["canvas", "black"]];
  }

  installCanvases() {
    this.canvases = [];
    var cvs = this.existingCanvases();
    for (var d of cvs) {
      var canvas = document.getElementById(d[0]);
      canvas.bg = d[1];
      this.canvases.push(canvas);
    }
  }

  installInput() {
    this.installMouseInput();
    this.installKeyboardInput();
  }

  installMouseInput() {
    this.canvases[0].addEventListener("click", this.onMouseDown.bind(this), false);
    this.canvases[0].addEventListener("mousemove", this.onMouseMove.bind(this), false);
  }

  installKeyboardInput() {
    window.addEventListener("keydown", this.onKeyDown.bind(this), true);
    window.addEventListener("keyup", this.onKeyUp.bind(this), true);

    this.key_down_map = {};
    this.key_depressing_map = {};
    this.key_pressing_map = {};
    this.key_up_map = {};

    this.key_map = {
      27: 'ESC',
      37: 'L1',
      38: 'U1',
      39: 'R1',
      40: 'D1',
      16: 'A1',
      83: 'L2',
      69: 'U2',
      68: 'D2',
      70: 'R2',
      90: 'A1',
      77: 'M1',
      78: 'S1',
      49: 'DEBUG1',
      50: 'DEBUG2',
      51: 'DEBUG3',
      52: 'DEBUG4'
    }
  }

  installTime() {
    this.now, this.dt, this.last = Date.now();
    this.dt = 0.00;

    this.rate = 250;
  }


  installLoops() {
    window.requestAnimationFrame(this.loop.bind(this));
  }

  installMusician() {
    this.musician = new Musician();
  }

  constructor(options) {
    this.installRendering();
    this.installInput();
    this.completeGameInstall();
  }

  completeGameInstall() {
    this.installGame();
    this.installTime();
    this.installLoops();
  }

  loop() {
    this.now = Date.now();
    var delta  = this.now - this.last;
    this.last = this.now;

    this.dt = this.dt + delta;

    if (this.dt < this.rate) {
      window.requestAnimationFrame(this.loop.bind(this));
      return;
    }

    while (this.dt > this.rate) {
      this.display.loop(delta);
      this.dt = 0;
    }
    
    this.draw();
    
    this.loopInput();

    window.requestAnimationFrame(this.loop.bind(this));
  }

  loopInput() {
    this.display.loopKeyboardInput(this.key_down_map, this.key_up_map, this.key_pressing_map, this.key_depressing_map);
  }

  draw() {
    for (var canvas of this.canvases) {
      this.setBackground(canvas);
    }

    for (var canvas of this.canvases) {
      this.display.draw(canvas, canvas.getContext('2d'));
    }  
  }

  setBackground(canvas) {
    var context = canvas.getContext('2d');
    context.clearRect(0, 0, canvas.width, canvas.height); //500
    context.fillStyle = canvas.bg;
    context.fillRect(0,0, canvas.width, canvas.height);
  }


  installGame() {

  }

  onKeyUp(event) {
    this.key_down_map[this.key_map[event.keyCode]] = false;
    this.key_up_map[this.key_map[event.keyCode]] = true;
  }

  onKeyDown(event) { 
    this.key_down_map[this.key_map[event.keyCode]] = true;
    this.key_up_map[this.key_map[event.keyCode]] = false;
  }

  onMouseUp(event) {
    var x = event.layerX;
    var y = event.layerY;
    this.game.onMouseUp(x, y);
  }

  onMouseDown(event) {
    // var x = event.layerX;
    // var y = event.layerY;
    this.display.clicked(event);
  }

  onMouseMove(event) {
    this.display.mouseMoved(event);
  }
}
