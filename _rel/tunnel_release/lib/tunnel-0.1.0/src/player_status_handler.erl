-module(player_status_handler).
-export([init/2]).

init(Req0, Opts) ->
  MainRoom = whereis(main_room),
  Name = cowboy_req:binding(name, Req0),
  res:res(Req0, Opts, s:s(MainRoom, {player_status, Name})).

  