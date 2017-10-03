"use strict";

class Hole extends Component {
  defaultInstalls() {
    return [Menu, TableView, TableMaster,
            Player, DisplayStatus];
  }

  receive(t, b) {
    if (t == 'joined') {
      // player..
      TableMaster.getPlayerStatus(b.player);
    }
  }

}