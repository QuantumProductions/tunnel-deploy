'use strict';

class Info extends View {
  draw(ctt, x, y, w, h, s) {
    switch (s.status) {
      // Command pattern?
      case "newcomer":
        this.showPlaying(ctt, x, y, w, h, s);
        break;
      case "searching":
        this.showFinding(ctt, x, y, w, h, s);
        break;
      default:
        return;
    }
  }

  showPlaying(ctt, x, y, w, h, s) {
    ctt.font = '80pt Courier New';
    ctt.fillStyle = Color.text;
    ctt.textAlign = 'left';
    let xal = 0.08 * w;
    let maxWidth = 0.5 * w;
    ctt.fillText("> Play", xal, 0.25 * h, w);
    ctt.fillText("> Spectate", xal, 0.5 * h, w);
    ctt.fillText("> Learn", xal, 0.75 * h, w);
  }

  showFinding(ctt, x, y, w, h, s) {
    ctt.font = '80pt Courier New';
    ctt.fillStyle = Color.text;
    ctt.textAlign = 'left';
    let xal = 0.08 * w;
    let maxWidth = 0.5 * w;
    ctt.fillText("< Cancel", xal, 0.25 * h, w);
    ctt.fillText(".. Finding Game", xal, 0.5 * h, w);
  }

  playStatus(p) {
    switch (p){
      case 0:
        return "Welcome";
      case 1:
        return "Finding Game";
      case 2:
        return "Now Playing";
      case 3:
        return "Spectating";
      default:
        return "MakeWay";
      }
  }


}