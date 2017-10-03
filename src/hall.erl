-module(hall).
-compile(export_all).

init([]) -> 
  {ok, []}.

go() ->
  gen_server:start_link(?MODULE, [], []).

handle_call(info, _, State) ->
  {reply, State, State};
handle_call({join, PlayerData}, _, Queue) ->
  Queue2 = lists:append(Queue, [PlayerData]),
  {reply, {ok, length(Queue2)}, Queue2};
handle_call(grab_player, _, [H | Queue]) ->
  {reply, {{ok, H}, Queue}, Queue};
handle_call(new_challenger, _, State) ->
% todo: notify
  {reply, State, State}.

terminate(normal, State) ->
    io:format("Hall.~p~n", [State]),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
handle_cast(_, State) ->
    {noreply, State}.
handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.