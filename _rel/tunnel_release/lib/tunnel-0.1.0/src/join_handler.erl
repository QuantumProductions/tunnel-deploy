-module(join_handler).
-export([init/2]).

init(Req0, Opts) ->
  MainRoom = whereis(main_room),
  Name = cowboy_req:binding(name, Req0),
  JoinRes = s:s(MainRoom, {join, #{name => Name}}),

  Res = io_lib:format("~p",[JoinRes]),
  Req = cowboy_req:reply(200, #{
          <<"content-type">> => <<"text/plain">>
  }, Res, Req0),
  {ok, Req, Opts}.