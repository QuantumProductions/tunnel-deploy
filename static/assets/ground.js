"use strict";

class ClickControls extends Component {
  init(o) {
    document.getElementById("canvas");
    this.addEventListener("click", this.clicked.bind(this), false);
  }

  clicked(e) {
    var click = {x: e.clientX, y: e.clientY};
    this.t.m('click', click);
  }
}

class Ground extends Component {
  defaultInstalls() {
    return [ClickControls];
  }
}