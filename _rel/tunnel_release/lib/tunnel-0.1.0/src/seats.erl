-module(seats).
-compile(export_all).
-behaviour(gen_server).

join(Seats, PlayerData) ->
  case maps:is_key(x, Seats) of
    true ->
      case maps:is_key(o, Seats) of
        true -> {error, seats_full};
        false -> {ok, o, maps:merge(#{o => PlayerData}, Seats)}
      end;
    false ->
      {ok, x, maps:merge(#{x => PlayerData}, Seats)}
  end.

leave(Seats, PlayerName) ->
  leave(Seats, PlayerName, x).
leave(Seats, PlayerName, x) ->
  case maps:is_key(x, Seats) of
    true ->
      case maps:get(name, maps:get(x, Seats)) == PlayerName of
        true ->  {ok, maps:remove(x, Seats)};
        false -> leave(Seats, PlayerName, o)
      end;
    false -> leave(Seats, PlayerName, o)
  end;
leave(Seats, PlayerName, o) ->
  case maps:is_key(o, Seats) of
    true ->
      case maps:get(name, maps:get(o, Seats)) == PlayerName of
        true ->  {ok, maps:remove(o, Seats)};
        false -> {not_playing, Seats}
      end;
    false -> {not_playing, Seats}
  end.

names(Seats) ->
  names(Seats, #{}).
names(Seats, Names) ->
  case maps:is_key(x, Seats) of
    true -> names(Seats, maps:merge(#{x => maps:get(name, maps:get(x, Seats))}, Names), o);
    false -> names(Seats, Names, o)
  end.
names(Seats, Names, o) ->
  case maps:is_key(o, Seats) of
    true -> maps:merge(#{o => maps:get(name, maps:get(o, Seats))}, Names);
    false -> Names
  end.

handle_call({valid, X, Name, Auth}, _, State) ->
  case maps:is_key(X, State) of
    true -> 
      PlayerData = maps:get(X, State),
      ExistingName = maps:get(name, PlayerData),
      ExistingAuth = maps:get(auth, PlayerData),
      case ExistingName == Name andalso Auth == ExistingAuth of
        true -> {reply, {ok, valid}, State};
        false -> {reply, {error, invalid_auth}, State}
      end;
    false -> {reply, {error, not_playing}, State}
  end;
handle_call(info, _, State) ->
  {reply, names(State), State};
handle_call(playable, _, State) ->
  {reply, length(maps:to_list(State)) =:= 2, State};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};
handle_call({join, PlayerData}, _From, State) ->
  Result = join(State, PlayerData),
  % TODO: Start timer on second join
  case Result of
    {error, Error} -> {reply, {error, Error}, State};
    {ok, Team, NewState} -> {reply, {ok, Team, names(NewState)}, NewState}
  end;
handle_call({leave, PlayerName}, _, State) ->
  case leave(State, PlayerName) of
    % TODO: send to From READY for table pid append by the ref
    {not_playing, State} -> {reply, {error, not_playing, names(State)}, State};
    {ok, NewState} -> {reply, {ok, names(NewState)}, NewState}
  end.

init([]) -> 
  {ok, maps:new()}.

terminate(normal, State) ->
    io:format("Seats.~p~n", [State]),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
handle_cast(_, State) ->
    {noreply, State}.
handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.

go() ->
  gen_server:start_link(?MODULE, [], []).
