"use strict";

class Table extends Component {
  defaultState() {
    return {tableId: null, source: TableMaster};
  }

  init(o) {
    //create timer
    //on timer, when table_id exists: getTableInfo from source.
    //This will be TableMaster, which will callback with table-update
    //when its TableProxy completes
  }

  defaultInstalls() {
    return [Board, Dashboard, TableProxy];
  }

  defaultTopics() {
    return ["table-update", "got-table-id"];
  }

  receive(t, b) {
    if (t == 'got-table-id') {
      this.tableId = b;
      return;
    }

    if (b.tableId == this.tableId) {
      this.tableInfo = b;
      this.m('board-update', b.board);
      var status = b.status;
      this.m('status-update', b.status);
    }
  }

  loop() {
    if (this.tableId) {
      //var proxy = this.grab('proxy');
      //proxy.getTableInfo(this.tableId);
    }

  }
}