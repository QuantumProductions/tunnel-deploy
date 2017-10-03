'use strict';

class View {
  constructor(x,y,w,h) {
    this.x = x;
    this.y = y;
    this.w = w;
    this.h = h;
  }

  draw(ctt, x, y, w, h, s) {

  }

  renderState(ctt, cnv, s) {
    const leftX = this.x * cnv.width;
    const topY = this.y * cnv.height;
    const width = this.w * cnv.width;
    const height = this.h * cnv.height;
    this.draw(ctt, leftX, topY, width, height, s);
  }
}