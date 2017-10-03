"use strict";

class TableView extends Component {
  init(o) {
    this.table = TableMaster.emptyTable();
  }

  defaultInstalls(o) {
    return [[DashboardView, {w: 0, h: 0}],
            BoardView];
  }

  loop() {
    var dashboard = this.grab('table').grab('dashboard');
    var dv = this.grab('dashboard-view');
    dv.m('update-dashboard', dashboard);
  }

}

class BoardView extends Component {
  defaultTopics() {
    return ['update-board', 'draw'];
  }

  process(t, b) {
    if (t == 'update-board') {
      this.board = b;
    } else {
      //render board
    }
}

class DashboardView extends Component {
// after initializing should add its children to interested topics
// and save to an array, iterating through and processing itself +
// each child component
  defaultTopics() {
    return ['update-dashboard', 'draw'];
  }

//old handle message will be replaced with 'process(t, b)'
//handleMessage will only be called by the component handling.
//with other messages passed to its child nodes to handleMessage
  process(t, b) {
    if (t == 'update-dashboard') {
      this.dashboard = b;
    } else {
      //canvas helper from b with t
      //
      //might call draw on global context using self's position
      //as offsets
      //
      //replaces context = b['context'];
      //grab will run recursively, if not found as a key here will
      //check child components
      var clock = this.dashboard.clock;
      var time = clock.getDisplayText();
      //render text
      var actions = this.dashboard.actions;
      var amount = actions.points;
      //render points as boxes
    }
  }

