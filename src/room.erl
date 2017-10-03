-module(room).
-compile(export_all).

randomString(Len) ->
    Chars = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890"),
    CharsSize = size(Chars),
    F = fun(_, R) -> [element(rand:uniform(CharsSize), Chars) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

generateAuth() -> binary:list_to_bin(randomString(20)).

join({Players, Tables, null}, #{name := ChallengerName}) ->
  PlayerData = #{name => ChallengerName, auth => generateAuth()},
  {{ok, PlayerData}, {Players, Tables, PlayerData}};
join({Players, Tables, #{name := ContenderName, auth := ContenderAuth}}, #{name := ChallengerName}) ->
  {ok, TablePid} = table:go(),
  Players2 = maps:put(ContenderName,  #{auth => ContenderAuth, status => playing, table_pid => TablePid},
    Players),
  ChallengerAuth = generateAuth(),
  Players3 = maps:put(ChallengerName, #{auth => ChallengerAuth, status => playing, table_pid => TablePid},
    Players2),
  TableCache = s:s(TablePid, info),
  Tables2 = maps:put(TablePid, #{seats => #{x => ContenderName, o => ChallengerName},
                                 cache => TableCache}, Tables),
  {{ok, #{name => ChallengerName, auth => ChallengerAuth}}, {Players3, Tables2, null}}.

validAuth(Players, Name, Auth) ->
  case maps:is_key(Name, Players) of
    true -> case maps:get(Name, Players) of
      #{auth := Auth} -> true;
      _ -> false
    end;
    false -> false
  end.

cancel(State = {_Players, _Tables, null}, _) ->
  {{error, bad_sequence}, State};
cancel(State, Canceller) ->
  cancel(matches, State, Canceller).
cancel(matches, State = {_Players, _Tables, #{name := ContenderName}}, Challenger = #{name := ChallengerName}) ->
  case ContenderName of
    ChallengerName -> cancel(validate, State, Challenger);
    _ ->  {{error, bad_sequence}, State}
  end;
cancel(validate, State = {Players, Tables, #{auth := ContenderAuth}}, #{auth := Auth}) ->
  case ContenderAuth == Auth of
    true -> {ok, {Players, Tables, null}};
    false -> {{error, invalid_auth}, State}
  end;
cancel(validate, State, _) ->
  {{error, invalid_auth}, State}.

status(Triple, #{name := Name}) ->
  case Triple of
    {_Players, _Tables, #{name :=Name}} -> #{status => challenging};
    {#{Name := PlayerStatus}, _, _} -> PlayerStatus;
    _ -> #{status => null}
  end.

valid_auth(Players, Name, Auth) ->
  #{auth := ExistingAuth, table_pid := TablePid} = maps:get(Name, Players),
  case ExistingAuth == Auth of
    true -> {true, TablePid};
    false -> false
  end.

play(1, State, TablePidBin, Player, Move) ->
  case validTable(TablePidBin) of
    {true, TablePid} -> play(2, State, TablePid, Player, Move);
    _ -> {error, invalid_table}
  end;
play(2, State, TablePid, Player = #{name := Name}, Move) ->
  case playerStatus(State, Name) of
    challenger -> {error, you_are_challenger};
    unknown -> {error, unknown_player};
    _Pid -> play(3, State, TablePid, Player, Move)
  end;
play(3, State = {_, Tables, _}, TablePid, Player, Move) ->
  case maps:is_key(TablePid, Tables) of
    true -> play(4, State, TablePid, Player, Move);
    false -> {error, invalid_table}
  end;
play(4, State = {Players, _, _}, TablePid, #{name := Name, auth := Auth}, Move) ->
  case validAuth(Players, Name, Auth) of
    true -> play(5, State, TablePid, Name, Move);
    false -> {error, invalid_auth}
  end;
play(5, {_, Tables, _}, TablePid, Name, Move) ->
  #{seats := Seats} = maps:get(TablePid, Tables),
  case seated(Seats, Name) of
    {error, Error} -> {error, Error};
    Team -> play(6, TablePid, Team, Move)
  end.
play(6, TablePid, Team, {Action, Position}) ->
  case validAction(Action) of
    false -> {error, invalid_action};
    ActionAtom -> play(7, TablePid, Team, {ActionAtom, Position})
  end;
play(7, TablePid, Team, {ActionAtom, Position}) ->
  case validPosition(Position) of
    {error, Error} -> {error, Error};
    PositionIntegers -> play(8, TablePid, Team, {ActionAtom, PositionIntegers})
  end;
play(8, TablePid, Team, {Action, Position}) ->
  s:s(TablePid, {place, Action, Team, Position}).

validCoordinate(C) when is_binary(C) ->
  try list_to_integer(binary_to_list(C)) of
    Value -> Value
  catch
    _:_ -> {error, bad_integer}
  end;
validCoordinate(C) when is_integer(C) ->
  0 < C andalso C < 6;
validCoordinate(_) -> {error, bad_integer}.

validPosition({X, Y}) ->
  case validCoordinate(X) of
    {error, Error} -> {error, Error};
    XInt ->
      case validCoordinate(Y) of
        {error, Error} -> {error, Error};
        YInt -> {XInt, YInt}
      end
  end.




validAction(Action) -> 
  case lists:member(Action, [<<"take">>, <<"east">>, <<"north">>, <<"south">>, <<"west">>]) of
    true -> list_to_atom(binary_to_list(Action));
    false -> false
  end.

seated(#{x := Name}, Name) -> x;
seated(#{o := Name}, Name) -> o;
seated(_, _) -> {error, not_seated}.

tableTeam(Pid, Tables, Player) ->
  #{seats := Seats} = maps:get(Pid, Tables),
  tableTeam(Seats, Player).

tableTeam(#{x := XName, o := _OName}, #{name := Name}) ->
  case XName == Name of
    true -> x;
    false -> o
  end.

  % validate player
  % validate action chosen
  % validate + convert coordinates
  % reply / error

processedPlay({error, Error}) -> res:hash(#{error => Error});
processedPlay({ok, Board}) -> 
  res:r(#{status => ok, board => Board}).  

validTable(TablePidBin) ->
  try list_to_pid(binary_to_list(TablePidBin)) of
    Pid ->
      {is_process_alive(Pid), Pid}
  catch
    _ -> false;
    _:_ -> false
  end.

atomJSON(Atom) -> binary:list_to_bin(atom_to_list(Atom)).
tablePidToJson(Pid) -> binary:list_to_bin(pid_to_list(Pid)).

processJoin(#{name := Name, auth := Auth}) -> %can check for null challenger
  binary:bin_to_list(jiffy:encode(#{<<"name">> => Name,
    <<"auth">> => Auth})).
processPlayerStatus(Atom) when is_atom(Atom) ->
  binary:bin_to_list(jiffy:encode(#{<<"status">> => atomJSON(Atom)}));
processPlayerStatus(TablePid) when is_pid(TablePid) ->
  binary:bin_to_list(jiffy:encode(#{<<"table_id">> => tablePidToJson(TablePid)})).
processTableInfo(invalid_table) ->
  res:hash(#{error => <<"invalid_table">>});
processTableInfo(Table) ->
  res:r(Table).

tableInfo({_, Tables, _}, TablePid) ->
  #{cache := Cache} = maps:get(TablePid, Tables),
  Cache.

playerStatus({Players, Tables, #{name := ChallengerName}}, Name) ->
  case ChallengerName == Name of
    true -> challenger;
    false -> playerStatus({Players, Tables, null}, Name)
  end;
playerStatus({Players, _Tables, _Challenger}, Name) ->
  case maps:is_key(Name, Players) of
    true -> 
      #{table_pid := TablePid} = maps:get(Name, Players),
      TablePid;
    false -> unknown
  end.

handle_call(debug, _, State) ->
  {reply, State, State};
handle_call({status, PlayerData}, _, State) ->
  R = status(State, PlayerData),
  {reply, R, State};
handle_call({cancel, PlayerData}, _, State) ->
  {Response, State2} = cancel(State, PlayerData),
  {reply, Response, State2};
handle_call({join, PlayerData}, _, State) ->
  {{ok, PlayerData2}, State2} = join(State, PlayerData),
  PlayerDataRes = processJoin(PlayerData2),
  {reply, PlayerDataRes, State2};
handle_call({play, TablePidBin, Player, Move}, _, State) ->
  Response = play(1, State, TablePidBin, Player, Move),
  {reply, processedPlay(Response), State};
handle_call({player_status, Name}, _, State) ->
  PlayerStatus = playerStatus(State, Name),
  PlayerStatusRes = processPlayerStatus(PlayerStatus),
  {reply, PlayerStatusRes, State};
handle_call({table_info, TablePidBin}, _, State) ->
  Res = case validTable(TablePidBin) of
    false -> invalid_table;
    {false, _} -> invalid_table;
    {true, Pid} -> tableInfo(State, Pid)
  end,
  Res2 = processTableInfo(Res),
  {reply, Res2, State};
handle_call({update, Delta}, _, State) ->
  State2 = update(tables, Delta, State),
  {reply, State2, State2}.

init([]) -> 
  Players = #{},
  Tables = #{},
  Challenger = null,
  {ok, {Players, Tables, Challenger}}.

go() ->
  {ok, Self} = gen_server:start_link(?MODULE, [], []),
  {ok, _Tref} = timer:apply_after(1000, ?MODULE, update, [start, Self, erlang:timestamp()]),
  {ok, Self}.

gameFinished(#{status := over}) -> true;
gameFinished(_) -> false.

removedTableData(TableData, Pid) ->
  case maps:is_key(Pid, TableData) of
    true ->
      {_, Remaining} = maps:take(Pid, TableData),
      Remaining;
    false ->
      TableData
  end.

update(tables, Delta, {Players, Tables, Challenger}) ->
  {Players2, Tables2} = update(Tables, Delta, Players),
  {ok, _Tref} = timer:apply_after(1000, ?MODULE, update, [start, self(), erlang:timestamp()]),
  {Players2, Tables2, Challenger};
update(start, Self, Then) ->
  Delta = timer:now_diff(erlang:timestamp(), Then) / 10000,
  s:s(Self, {update, Delta});
update(Tables, Delta, Players) ->
  update(maps:keys(Tables), Tables, Delta, Players, #{}).

update([], _TableData, _Delta, Players, Cache) ->
  {Players, Cache};
update([ HPid | T], TableData, Delta, Players, Cache) ->
  Info = #{status := TableStatus} = s:s(HPid, info),
% need to mark player status?
  case gameFinished(TableStatus) of
    true ->
        % gen_server:stop(HPid),
        update(T, TableData, Delta, Players, Cache);
    false ->
      OldTableInfo = maps:get(HPid, TableData),
      NewTableInfo = maps:put(cache, Info, OldTableInfo),
      Cache2 = maps:put(HPid, NewTableInfo, Cache),
      update(T, TableData, Delta, Players, Cache2)
  end.
