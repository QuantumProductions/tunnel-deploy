-module(table_test).
-include_lib("eunit/include/eunit.hrl").

% kills_board_test() ->
%   {ok, Table} = table:go(),
%   {BoardPid, _ , Players} = s:s(Table, info),
%   #{x := {XClock, XActions}, o := {OClock, OActions}} = Players,
%   true = is_process_alive(BoardPid),
%   true = is_process_alive(XClock),
%   true = is_process_alive(XActions),
%   true = is_process_alive(OClock),
%   true = is_process_alive(OActions),
%   gen_server:stop(Table),
%   undefined = process_info(XClock),
%   undefined = process_info(XActions),
%   undefined = process_info(OClock),
%   undefined = process_info(OActions).
  % undefined = process_info(BoardPid).

% win_game_marked_test() -> 
%   time_dependent = false.

% default_board_test() ->
%   {ok, Table} = table:go(),
%   DefaultBoard = board:default(),
%   #{board := DefaultBoard} = s:s(Table, info).

% x_takes_2_1_test() ->
%   {ok, Table} = table:go(),
%   s:s(Table, {place, take, x, {2, 1}}),
%   {BoardPid, _, _} = s:s(Table, info),
%   [Row1 | _] = s:s(BoardPid, info),
%   [{spawn, x, none}, {recent, x, none}, _, _, _]  = Row1.

% first_turn_action_swap_test() ->
%   {ok, Table} = table:go(),
%   s:s(Table, {place, take, x, {2, 1}}),
%   {_, #{current_player := o}, _} = s:s(Table, info).

% out_of_order_test() ->
%   {ok, Table} = table:go(),
%   s:s(Table, {place, take, o, {4, 5}}),
%   {_, #{current_player := x}, _} = s:s(Table, info).

% regular_play_test() ->
%   {ok, Table} = table:go(),
%   s:s(Table, {place, take, x, {2, 1}}),
%   s:s(Table, {place, take, o, {4, 5}}),
%   s:s(Table, {place, take, o, {4, 4}}),
%   s:s(Table, {place, take, x, {3, 1}}),
%   s:s(Table, {place, take, x, {3, 2}}),
%   {_, #{current_player := o}, _} = s:s(Table, info).

% regular_play_2_test() ->
%   {ok, Table} = table:go(),
%   s:s(Table, {place, take, x, {2, 1}}),
%   s:s(Table, {place, take, o, {4, 5}}),
%   {_, #{current_player := o}, _} = s:s(Table, info).

% bad_move_test() ->
%   {ok, Table} = table:go(),
%   s:s(Table, {place, take, x, {2, 1}}),
%   s:s(Table, {place, take, o, {4, 5}}),
%   s:s(Table, {place, take, o, {4, 4}}),
%   s:s(Table, {place, take, x, {3, 1}}),
%   s:s(Table, {place, take, x, {3, 4}}),
%   {_, #{current_player := x}, #{x := {_, XActions}}} = s:s(Table, info),
%   {2,1} = s:s(XActions, info).

% slices_test() ->
%   {ok, Table} = table:go(),
%   s:s(Table, {place, take, x, {2, 1}}),
%   s:s(Table, {place, take, o, {5, 4}}),
%   s:s(Table, {place, take, o, {5, 3}}),
%   s:s(Table, {place, take, x, {3, 1}}),
%   s:s(Table, {place, take, x, {3, 2}}),
%   s:s(Table, {place, take, o, {4, 3}}),
%   s:s(Table, {place, take, o, {3, 3}}),
%   s:s(Table, {place, take, x, {4, 2}}),
%   s:s(Table, {place, take, x, {4, 3}}),
%   #{players := Players} = s:s(Table, info),
%   #{x := {_, XActions}, o := {_, OActions}} = Players,
%   {2, 3} = s:s(XActions, info),
%   {2, 3} = s:s(OActions, info).

wall_test() ->
  {ok, Table} = table:go(),
  s:s(Table, {place, take, x, {2, 1}}),
  s:s(Table, {place, take, o, {5, 4}}),
  s:s(Table, {place, take, o, {5, 3}}),
  s:s(Table, {place, take, x, {3, 1}}),
  s:s(Table, {place, east, x, {3, 1}}),
  s:s(Table, {place, take, o, {5, 2}}),
  s:s(Table, {place, take, o, {5, 1}}),
  s:s(Table, {place, take, x, {1, 2}}),
  s:s(Table, {place, take, x, {1, 3}}),
  s:s(Table, {place, take, o, {4, 1}}),
  Res = s:s(Table, {place, take, o, {3, 1}}),
  Res.

% win_test() ->
%   {ok, Table} = table:go(),
%   s:s(Table, {place, take, x, {2, 1}}),
%   s:s(Table, {place, take, o, {5, 4}}),
%   s:s(Table, {place, take, o, {5, 3}}),
%   s:s(Table, {place, take, x, {3, 1}}),
%   s:s(Table, {place, take, x, {3, 2}}),
%   s:s(Table, {place, take, o, {5, 2}}),
%   s:s(Table, {place, take, o, {5, 1}}),
%   s:s(Table, {place, take, x, {3, 3}}),
%   s:s(Table, {place, take, x, {2, 3}}),
%   s:s(Table, {place, take, o, {4, 1}}),
%   s:s(Table, {place, take, o, {3, 1}}),
%   s:s(Table, {place, take, x, {1, 2}}),
%   s:s(Table, {place, take, x, {1, 3}}),
%   s:s(Table, {place, take, x, {1, 4}}),
%   s:s(Table, {place, take, o, {2, 1}}),
%   s:s(Table, {place, take, o, {1, 1}}),
%   #{status := #{result := owin}} = s:s(Table, info).

% % % timeout_x_test() ->
% % %   {ok, Table} = table:go(),
% % %   s:s(Table, {timeout, x}),
% % %   s:s(Table, {timeout, o}),
% % %   % o's timeout should be i gnored
% % %   #{status := #{status := owin}} = s:s(Table, info).
