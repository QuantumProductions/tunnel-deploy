-module(tunnel_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
    {"/info", lobby_handler, []},
    {"/join/:name", join_handler, []},
    {"/player/status/:name", player_status_handler, []},
    {"/tables/info/:table_id", table_info_handler, []},
    {"/tables/play/:table_id/:name/:auth/:action/:x/:y", table_play_handler, []},
    {"/assets/[...]", cowboy_static, {dir, "/users/quantum/tunnel/static/assets/"}}
    ]}]),
  {ok, _} = cowboy:start_clear(my_http_listener, 100,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}),
  {ok, MainDoor} = door:go(),
  register(main_door, MainDoor),
  {ok, MainRoom} = room:go(),
  register(main_room, MainRoom),
  tunnel_sup:start_link().

stop(_State) ->
	ok.


