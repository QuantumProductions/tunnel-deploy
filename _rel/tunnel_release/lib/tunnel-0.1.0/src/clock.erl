-module(clock).
-compile(export_all).

-define(STARTING, 24000).
-define(INTERVAL, 250).

init([Host]) -> 
  {ok, defaultState(Host)}.

defaultState(Host) ->
  {{?STARTING, paused, none}, Host}.

tickPid(Pid, Then) ->
  Delta = timer:now_diff(erlang:timestamp(), Then) / 10000,
  s:s(Pid, {tick, Delta}).
  
intervalRef() ->
 {ok, {_, Tref}} = timer:apply_after(?INTERVAL, ?MODULE, tickPid, [self(), erlang:timestamp()]),
 Tref.

updateTick(Delta, {{Time, ticking, _}, Host}) ->
  Now = Time - Delta,
  case Now of
    Now when Now > 0 ->
      {{Now, ticking, intervalRef()}, Host};
    _ ->
      s:s(Host, clockdone),
      {{0, out, none}, Host}
  end;

updateTick(_, State) ->
  State.

handle_call({tick, Delta}, _, State) ->
  State2 = updateTick(Delta, State),
  {reply, State2, State2};
handle_call(info, _, State) ->
  {reply, State, State};
handle_call(pause, _, {{Time, ticking, Tref}, Host}) ->
  timer:cancel(Tref),
  State2 = {{Time, paused, none}, Host},
  {reply, State2, State2};
handle_call(start, _, {{Time, paused, _}, Host}) ->
  {ok, Tref} = timer:apply_after(?INTERVAL, ?MODULE, tickPid, [self(), erlang:timestamp()]),
  State2 = {{Time, ticking, Tref}, Host},
  {reply, State2, State2};
handle_call(stop, _From, State) ->
  {stop, normal, shutdown_ok, State};
handle_call(_, _, State) ->
  {reply, State, State}.

terminate(_, State = {{_,_, none}, _}) ->
    io:format("Clock.~p~n", [State]),
    ok;
terminate(_, State = {{_,_,Tref}, _}) ->
    timer:cancel(Tref),
    io:format("Clock.~p~n", [State]),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
handle_cast(_, State) ->
    {noreply, State}.
handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State }.

go(Host) ->
  gen_server:start_link(?MODULE, [Host], []).
