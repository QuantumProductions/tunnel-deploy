-module(door).
-compile(export_all).
-behaviour(gen_server).

validTable(TablePidString) ->
  try list_to_pid(TablePidString) of
    Pid ->
      Pid
  catch
    _ -> false;
    _:_ -> false
  end.

randomString(Len) ->
    Chars = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890"),
    CharsSize = size(Chars),
    F = fun(_, R) -> [element(rand:uniform(CharsSize), Chars) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

registerPlayer(Name) ->
% Name is sent as random from client
% Uniqueness is not required;
% However this is where login for ELO would take place.
  #{name => Name,
   auth => binary:list_to_bin(randomString(20))}.

tablePidToJson(Pid) -> binary:list_to_bin(pid_to_list(Pid)).

atomJSON(Atom) -> binary:list_to_bin(atom_to_list(Atom)).

actionsJSON({
  #{x := {XCurrent, XNext}, o := {OCurrent, ONext}}, Current}) ->
    #{<<"x">> => #{<<"now">> => XCurrent,
             <<"next">> => XNext},
    <<"o">> => #{<<"now">> => OCurrent,
             <<"next">> => ONext},
    <<"current">> => atomJSON(Current)}.

clockJSON({#{x := XTime, o := OTime}, Current, Status}) ->
  #{<<"x">> => XTime, <<"o">> => OTime,
    <<"current">> => atomJSON(Current),
    <<"status">> => atomJSON(Status)};
clockJSON({#{x := XTime, o := OTime}, Current, Status, _OverTime}) ->
  clockJSON({#{x => XTime, o => OTime}, Current, Status}).

tileJSON({Status, Sitter, Wall}) ->
  #{<<"status">> => atomJSON(Status), 
  <<"owner">> => atomJSON(Sitter),
  <<"wall">> => atomJSON(Wall)}.

rowJSON(Row) ->
  lists:map(fun(Tile) -> tileJSON(Tile) end, Row).

boardJSON(Board) ->
  lists:map(fun(Row) -> rowJSON(Row) end, Board).

tableCacheJSON(Cache) ->
  tableCacheJSON(maps:keys(Cache), Cache, #{}).
tableCacheJSON([], _Cache, CacheJSON) ->
  CacheJSON;
tableCacheJSON([H | TPid], Cache, CacheJSON) ->
  tableCacheJSON(TPid, Cache,
  maps:put(tablePidToJson(H), tableCacheJSON(single, maps:get(H, Cache)), CacheJSON)).
tableCacheJSON(single, #{actions := Actions, clock := Clock, board := Board, seats := Seats}) ->
    #{<<"actions">> => actionsJSON(Actions),
      <<"clock">> => clockJSON(Clock),
      <<"board">> => boardJSON(Board),
      <<"seats">> => seatsJSON(Seats)
      };
tableCacheJSON(single, _) ->
  #{<<"debug">> => <<"true">>}.

parseInfo({Cache, _, _HallPid, _}) ->
  binary:bin_to_list(jiffy:encode(#{<<"tableCache">> => tableCacheJSON(Cache)})).
  
seatsJSON(Seats) ->
 % Convert map to JSON dictionary
 Seats.

updateState(player_searching, Name, {RoomPid, Statuses}) ->
  {RoomPid, maps:put(Name, #{status => searching}, Statuses)}.
updateState(playing, Name, TablePid, {RoomPid, Statuses}) ->
  {RoomPid, maps:put(Name, #{status => playing, table_pid => TablePid}, Statuses)}.

processJoin(ok, #{name := Name, auth := Auth}, State) ->
  {binary:bin_to_list(jiffy:encode(#{<<"name">> => Name,
    <<"auth">> => Auth})), updateState(player_searching, Name, State)};
processJoin({table_joined, TablePid, #{name := Name, auth := Auth}, PlayerName2}, _, State) ->
  State2 = updateState(playing, Name, TablePid, State),
  State3 = updateState(playing, PlayerName2, TablePid, State2),
  {binary:bin_to_list(jiffy:encode(#{<<"name">> => Name, <<"auth">> => Auth})),
  State3}.
  
parseCancel(ok) ->
  <<"ok">>.
  
parsePlay({ok, #{actions := Actions, board := Board, clock := Clock}}) ->
  binary:bin_to_list(jiffy:encode(
    #{<<"actions">> => actionsJSON(Actions),
    <<"board">> => boardJSON(Board),
    <<"clock">> => clockJSON(Clock),
    <<"winner">> => <<"none">> }));
parsePlay({win, #{board := Board, winner := Winner}}) ->
  binary:bin_to_list(jiffy:encode(
    #{<<"winner">> => atomJSON(Winner),
      <<"board">> => boardJSON(Board)}));
parsePlay({error, Error}) ->
  binary:bin_to_list(jiffy:encode(
    #{<<"error">> => atomJSON(Error)}
    )).

parseStatus(#{status := Status, table_pid := TablePid}) ->
  binary:bin_to_list(jiffy:encode(
    #{<<"status">> => atomJSON(Status),
      <<"table_id">> => tablePidToJson(TablePid)}));
parseStatus(#{status := Status}) when is_atom(Status) ->
  binary:bin_to_list(jiffy:encode(
    #{<<"status">> => atomJSON(Status)}));
parseStatus(_) ->
  unknown.

statusResponse(Statuses, PlayerName) ->
  case maps:is_key(PlayerName, Statuses) of
    true -> parseStatus(maps:get(PlayerName, Statuses));
    false -> binary:bin_to_list(jiffy:encode(
      #{<<"status">> => <<"newcomer">>}))
  end.

cancel(Statuses, Name) ->
  case maps:take(Name, Statuses) of
    error -> Statuses;
    {_, Statuses2} -> Statuses2
  end.

handle_call({player_status, PlayerName}, _, State) ->
  {_Room, Statuses} = State,
  Res = statusResponse(Statuses, PlayerName),
  {reply, Res, State};
handle_call(debug, _, State) ->
  {reply, State, State};
handle_call({info, TablePidString}, _, State) ->
  {Room, _} = State,
  case validTable(TablePidString) of
    false ->
      {reply, {error, invalid_table}, Room};
    TablePid ->
      TableInfo = tableCacheJSON(single, (s:s(Room, {table_status, TablePid}))),
      Res = binary:bin_to_list(jiffy:encode(TableInfo)),
      {reply, Res, State}
  end;
handle_call(info, _, State) ->
  {Room, _} = State,
  {reply, parseInfo(s:s(Room, info)), State};
handle_call({play, TablePidString, MoveInfo, Authentication}, _, State) ->
  {Room, _} = State,
  case validTable(TablePidString) of
    false -> {reply, {error, invalid_table}, Room};
    % Validate Move Info is ok
    % /play/:roompid/:tablepid/{x,o}/{name, auth}/{take,wall}/{1-5}/{1-5}
    % Validate roompid
    TablePid -> {reply, parsePlay(s:s(Room, {play, TablePid, MoveInfo, Authentication})), State}
  end;
handle_call({join, Name}, _, State) ->
  {Room, _} = State,
  PlayerData = registerPlayer(Name),
  {Response, State2} = processJoin(s:s(Room, {join, PlayerData}), PlayerData, State),
  {reply, Response, State2};
handle_call({cancel, Name, Auth}, _, State) ->
  {Room, Statuses} = State,
  Statuses2 = cancel(Statuses, Name),
  {reply, parseCancel(s:s(Room, {cancel, Name, Auth})), {Room, Statuses2}};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 

go() ->
  gen_server:start_link(?MODULE, [], []).

handle_cast(_, State) ->
  {noreply, State}.
handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State }.

terminate(normal, State) ->
    io:format("Game.~p~n", [State]),
    ok.

init([]) -> 
  % Todo: erase old player maps
  {ok, RoomPid} = room:go(),
  {ok, {RoomPid, #{}}}.
