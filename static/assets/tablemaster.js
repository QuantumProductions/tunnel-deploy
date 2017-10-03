"use strict";

class TableMaster {
  static getTableInfo(t, table_id) {
    TableMaster.t = t;
    http.get({
      url: "http://localhost:8080/tables/status/" + tableName,
      onload: this.handleTableInfo.bind(this)
    });    
  }

  handleTableInfo(res) {
    var json = JSON.parse(res);
    TableMaster.t.m('table-update', json);
  }

  static emptyTable() {
    return new Table();
  }
}