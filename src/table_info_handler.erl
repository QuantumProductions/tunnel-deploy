-module(table_info_handler).

-export([init/2]).

init(Req0, Opts) ->
  MainRoom = whereis(main_room),
  % TableID = binary_to_list(cowboy_req:binding(table_id, Req0)),
  TableID = cowboy_req:binding(table_id, Req0),
  res:res(Req0, Opts, s:s(MainRoom, {table_info, TableID})).