-module(room_test).
-include_lib("eunit/include/eunit.hrl").

join_test() ->
  {ok, Room} = room:go(),
  s:s(Room, {join, #{name => "Marisfrolg"}}),
  {#{}, #{}, #{name := "Marisfrolg"}} = s:s(Room, debug),
  s:s(Room, {join, #{name => "Blandline"}}),
  {Players, Tables, Challenger} = s:s(Room, debug),
  [FirstPid] = maps:keys(Tables),
  #{"Marisfrolg"  := #{status := playing, table_pid := FirstPid},
   "Blandline"    := #{status := playing, table_pid := FirstPid}} = Players,
  #{FirstPid      := #{seats  := #{x := "Marisfrolg", o := "Blandline"},
                       cache := #{status := #{status := playing}}}} = Tables,
   null = Challenger.

cancel_authorized_test() ->
  {ok, Room} = room:go(),
  {ok, #{auth := Auth}} = s:s(Room, {join, #{name => "Marisfrolg"}}),
  {#{}, #{}, #{name := "Marisfrolg"}} = s:s(Room, debug),
  s:s(Room, {cancel, #{name => "Marisfrolg"}}),
  {_, _, #{name := "Marisfrolg"}} = s:s(Room, debug),
  s:s(Room, {cancel, #{name => "Marisfrolg", auth => Auth}}),
  {_, _, null} = s:s(Room, debug).

status_test() ->
  {ok, Room} = room:go(),
  s:s(Room, {join, #{name => "Marisfrolg"}}),
  #{status := challenging} = s:s(Room, {status, #{name => "Marisfrolg"}}),
  #{status := null} = s:s(Room, {status, #{name => "Tremulous.net"}}),
  s:s(Room, {join, #{name => "Blandline"}}),
  {_, Tables, _} = s:s(Room, debug),
  [FirstPid] = maps:keys(Tables),
  #{status := playing, table_pid := FirstPid} = s:s(Room, {status, #{name => "Marisfrolg"}}),
  #{status := playing, table_pid := FirstPid} = s:s(Room, {status, #{name => "Blandline"}}).

invalid_auth_test() ->
  {ok, Room} = room:go(),
  {ok, #{auth := _Auth}} = s:s(Room, {join, #{name => "Marisfrolg"}}),
  s:s(Room, {join, #{name => "Blandline"}}),
  {error, invalid_auth} = s:s(Room, {play, #{name => "Marisfrolg", auth => "badauth"}, useless_move}).

invalid_input_test() ->
  {ok, Room} = room:go(),
  {ok, #{auth := Auth}} = s:s(Room, {join, #{name => "Marisfrolg"}}),
  s:s(Room, {join, #{name => "Blandline"}}),
  {error, invalid_input} = s:s(Room, {play, #{name => "Marisfrolg", auth => Auth}, {bad, move}}).

out_of_order_test() ->
  {ok, Room} = room:go(),
  s:s(Room, {join, #{name => "Marisfrolg"}}),
  {ok, #{auth := Auth}} = s:s(Room, {join, #{name => "Blandline"}}),
  {error, out_of_order} = s:s(Room, {play, #{name => "Blandline", auth => Auth}, {take, {4,5}}}).

finished_game_test() ->
  {ok, Room} = room:go(),
  {ok, #{auth := AX}} = s:s(Room, {join, #{name => "Marisfrolg"}}),
  {ok, #{auth := AO}} = s:s(Room, {join, #{name => "Blandline"}}),
  {_Players, Tables, _Challenger} = s:s(Room, debug),
  [FirstPid] = maps:keys(Tables),
  s:s(Room, {play, #{name => "Marisfrolg", auth => AX}, {take, {2, 1}}}),
  s:s(Room, {play, #{name => "Blandline", auth => AO}, {take, {5, 4}}}),
  s:s(Room, {play, #{name => "Blandline", auth => AO}, {take, {5, 3}}}),
  s:s(Room, {play, #{name => "Marisfrolg", auth => AX}, {take, {3, 1}}}),
  s:s(Room, {play, #{name => "Marisfrolg", auth => AX}, {take, {3, 2}}}),
  s:s(Room, {play, #{name => "Blandline", auth => AO}, {take, {5, 2}}}),
  s:s(Room, {play, #{name => "Blandline", auth => AO}, {take, {5, 1}}}),
  s:s(Room, {play, #{name => "Marisfrolg", auth => AX}, {take, {3, 3}}}),
  s:s(Room, {play, #{name => "Marisfrolg", auth => AX}, {take, {4, 3}}}),
  s:s(Room, {play, #{name => "Blandline", auth => AO}, {take, {4, 1}}}),
  s:s(Room, {play, #{name => "Blandline", auth => AO}, {take, {3, 1}}}),
  s:s(Room, {play, #{name => "Marisfrolg", auth => AX}, {take, {1, 2}}}),
  s:s(Room, {play, #{name => "Marisfrolg", auth => AX}, {take, {1, 3}}}),
  s:s(Room, {play, #{name => "Marisfrolg", auth => AX}, {take, {1, 4}}}),
  s:s(Room, {play, #{name => "Blandline", auth => AO}, {take, {2, 1}}}),
  s:s(Room, {play, #{name => "Blandline", auth => AO}, {take, {1, 1}}}),
  s:s(Room, {update, 1000}),
  {_Players, #{FirstPid := #{status := #{status := owin}}}, _Challenger} = s:s(Room, debug).
  
% timeout_game_test() ->
%   {ok, Room} = room:go(),
%   {ok, #{auth := AX}} = s:s(Room, {join, #{name => "Marisfrolg"}}),
%   {ok, #{auth := AO}} = s:s(Room, {join, #{name => "Blandline"}}),  
%   {_Players, Tables, _Challenger} = s:s(Room, debug),
%   [FirstPid] = maps:keys(Tables),
%   timer:sleep()