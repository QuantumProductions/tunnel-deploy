"use strict";

class PlayerPanel extends Component {
  init(o) {
    this.team = o['team'];
    this.install('clock', new Clock());
    this.install('actions', new Actions());
  }

  interestedTopics() {
    return ['table_update'];
  }

  handleMessage(t, b) {
    if (t.team == this.team) {
      var clock = this.grab('clock');
      clock.time = t.players[t.team].time;
      var actions = this.grab('actions');
      actions.points = t.players[t.team].points;
      //this class only updates data
    }
  }
}

class Dashboard extends Thing {
  defaultInstalls() {
    return 
      [[PlayerPanel, {team: "x"}],
       [PlayerPanel, {team: "o"}]];
  }
}
