-module(table_play_handler).
-export([init/2]).

init(Req0, Opts) ->
  MainRoom = whereis(main_room),
  Name = cowboy_req:binding(name, Req0),
  Auth = cowboy_req:binding(auth, Req0),
  Player = #{name => Name, auth => Auth},
  Action = cowboy_req:binding(action, Req0),
  TablePidBin = cowboy_req:binding(table_id, Req0),
  X = cowboy_req:binding(x, Req0),
  Y = cowboy_req:binding(y, Req0),
  Move = {Action, {X, Y}},
  res:res(Req0, Opts, s:s(MainRoom, {play, TablePidBin, Player, Move})).

  