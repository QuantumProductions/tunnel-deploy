-module(actions).
-compile(export_all).
-define(RESET, 2).
-include_lib("eunit/include/eunit.hrl").

init([Starting]) -> 
  {ok, defaultState(Starting)}.

defaultState(Starting) ->
  {2, Starting}.

% TODO: Account for Max Move from instant_bonus
maxMove(N) when N >= 4 -> 4;
maxMove(N) -> N.
% TODO: Account for Max Move from {made, Slice}

handle_call({recent_bonus, Bonus}, _, {Next, Current}) ->
  State2 = {Next, Current + Bonus},
  {reply, {ok, State2}, State2};
handle_call({made, Slices}, _, {Next, 1}) ->
  State2 = {?RESET, Next + Slices},
  {reply, {over, State2}, State2};
handle_call({made, Bonus}, _, {Next, Current}) ->
  State2 = {Next + Bonus, Current - 1},
  {reply, {ok, State2}, State2};
handle_call(stop, _From, State) ->
  {stop, normal, shutdown_ok, State};
handle_call(_, _, State) ->
  {reply, State, State}.

terminate(normal, State) ->
    io:format("Clock.~p~n", [State]),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
handle_cast(_, State) ->
    {noreply, State}.
handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State }.

go(Starting) ->
  gen_server:start_link(?MODULE, [Starting], []).

decrement_test() ->
  {ok, A} = actions:go(1),
  {over, {2, 2}} = s:s(A, {made, 0}).

made_bonus_test() ->
  {ok, A} = actions:go(2),
  {ok, {3, 1}} = s:s(A, {made, 1}).

recent_contest_bonus_test() ->
  {ok, A} = actions:go(2),
  {ok, {2, 3}} = s:s(A, {recent_bonus, 1}).

swap_turn_test() ->
  {ok, A} = actions:go(2),
  {ok, B} = actions:go(2),
  {ok, {3, 1}} = s:s(A, {made, 1}),
  {ok, {2, 3}} = s:s(B, {recent_bonus, 1}),
  {over, {2, 4}} = s:s(A, {made, 1}),
  {ok, {3, 2}} = s:s(B, {made, 1}).