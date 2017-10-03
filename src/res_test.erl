-module(res_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

map_test() ->
  Map = #{a => 1},
  res:r(Map).

table_test() ->
  {ok, TablePid} = table:go(),
  Info = s:s(TablePid, info),
  res:r(Info).

actions_test() ->
  Actions = {#{o => {0,0},x => {1,0}},x,{#{o => 2400,x => 2374.9464},started}},
  res:r(Actions).