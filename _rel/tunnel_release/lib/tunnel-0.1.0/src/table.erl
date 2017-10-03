-module(table).
-compile(export_all).
-behavior(gen_server).

-define(POSTGAME_TIMEOUT, 6000).

otherPlayer(x) -> o;
otherPlayer(o) -> x.

processRecentTaken(true) -> 1;
processRecentTaken(false) -> 0.

processResult({error, Error, _Board}, State) ->
  {{error, Error}, State};
processResult({playing, NewBoard, Slices, RecentTaken},   {Board, Status, Players}) ->
  % update recent taken
  CurrentPlayer = maps:get(current_player, Status),
  OtherPlayer = otherPlayer(CurrentPlayer),
  {OClock, OActions} = maps:get(OtherPlayer, Players),
  s:s(OActions, {recent_bonus, processRecentTaken(RecentTaken)}),
  % update slice bonus
  {CClock, CActions} = maps:get(CurrentPlayer, Players),
  Made = s:s(CActions, {made, Slices}),
  Status2 = case Made of
    {over, _} ->
      s:s(Board, {cycle, OtherPlayer}),
      s:s(CClock, pause),
      s:s(OClock, start),
      maps:put(current_player, OtherPlayer, Status);
    _ -> Status
  end,
  {{ok, NewBoard}, {Board, Status2, Players}};
processResult({Win, NewBoard, _Slices, _RecentTaken}, State) ->
  {{ok, NewBoard}, winGame(Win, State)}.

markAsFinished(Pid, _Timestamp) ->
  s:s(Pid, finished).

winGame(x, State) ->
  winGame(xWin, State);
winGame(o, State) ->
  winGame(oWin, State);
winGame(Win, {Board, Status, Players}) ->
  CurrentPlayer = maps:get(current_player, Status),
  OtherPlayer = otherPlayer(CurrentPlayer),
  {OClock, _} = maps:get(OtherPlayer, Players),
  CurrentPlayer = maps:get(current_player, Status),
  {CClock, _} = maps:get(CurrentPlayer, Players),
  s:s(OClock, pause),
  s:s(CClock, pause),
  Status2 = maps:put(result, Win, Status),
  % {ok, _Tref} = timer:apply_after(?POSTGAME_TIMEOUT, ?MODULE, markAsFinished, [self(), erlang:timestamp()]),
  {Board, Status2, Players}.

handle_call({place, Action, Player, Position}, _, State = {Board, Status, _Players}) ->
% TODO: check for is playing
  CurrentPlayer = maps:get(current_player, Status),
  Result = s:s(Board, {place, CurrentPlayer, {Action, Player, Position}}),
  {Response, State2} = processResult(Result, State),
  {reply, Response, State2};
handle_call(clockdone, {TimeoutPid, _}, State = {_,_, Players}) ->
  TimeoutPlayer = getPlayerForClockPid(TimeoutPid, Players),
  {reply, {ok, TimeoutPlayer}, State};
% handle_call(clockdone, {TimeoutPid, _Tag}, State = {_Board, _Status, Players}) ->
%   % {reply, State, State};
%   TimeoutPlayer = getPlayerForClockPid(TimeoutPid, Players),
%   WinningPlayer = otherPlayer(TimeoutPlayer),
%   {Res, State2} = winGame(WinningPlayer, State),
%   {reply, Res, State2};
handle_call(finished, _, {Board, Status, Players}) ->
  Status2 = maps:put(result, finished, Status),
  State2 = {Board, Status2, Players},
  {reply, State2, State2};
handle_call(assign_players, _, {Board, Status}) ->
  Players = createPlayers(self()),
  State2 = {Board, Status, Players},
  {reply, State2, State2};
handle_call(info, _, State = {Board, Status, #{x := X, o := O}}) ->
  BoardInfo = s:s(Board, info),
  RX = playerInfo(X),
  RO = playerInfo(O),
  Res = #{board => BoardInfo,
          status => Status, 
          players => #{x => RX, o => RO}},
  {reply, Res, State};
handle_call(_, _, State) ->
  {reply, State, State}.

playerInfo({Clock, Actions}) ->
  {Next, Current} = s:s(Actions, info),
  {{Time, _ ,_}, _} = s:s(Clock, info),
  #{clock => Time, actions => #{next => Next, current => Current}}.

getPlayerForClockPid(ClockPid, Players) ->
  getPlayerForClockPid(ClockPid, Players, maps:keys(Players)).
getPlayerForClockPid(ClockPid, Players, [H | T]) ->
  case maps:get(H, Players) of
    {ClockPid, _} -> H;
    _ -> getPlayerForClockPid(ClockPid, Players, T)
  end.

actionProcess(x) -> actions:go(1);
actionProcess(o) -> actions:go(2).

playerProcesses(Pid, Player) ->
  {ok, Clock} = clock:go(Pid),
  {ok, Actions} = actionProcess(Player),
  {Clock, Actions}.

playerNames() ->
  [x, o].

createPlayers(Self) ->
  createPlayers(Self, playerNames(), #{}).
createPlayers(_Self, [], Players) ->
  Players;
createPlayers(Self, [H | T], Players) ->
  createPlayers(Self, T, maps:put(H, playerProcesses(Self, H), Players)).

defaultStatus() ->
  #{current_player => x,
    result => playing}.

init([]) -> 
  {ok, Board} = board:go(),
  Status = defaultStatus(),
  {ok, {Board, Status}}.

go() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  s:s(Pid, assign_players),
  {ok, Pid}.

terminate(_, State = {_Board, Status, Players}) ->
  % gen_server:stop(Board),
  CurrentPlayer = maps:get(current_player, Status),
  OtherPlayer = otherPlayer(CurrentPlayer),
  {OClock, OActions} = maps:get(OtherPlayer, Players),
  CurrentPlayer = maps:get(current_player, Status),
  {CClock, CActions} = maps:get(CurrentPlayer, Players),
  gen_server:stop(OClock),
  gen_server:stop(CClock),
  gen_server:stop(OActions),
  gen_server:stop(CActions),
  io:format("Table Terminating.~p~n", [State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 

handle_cast(_, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.
