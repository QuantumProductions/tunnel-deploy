-module(res).
-export([res/3, hash/1, r/1]).

res(Req0, Opts, Info) ->
  Res = io_lib:format("~p",[Info]),
  Req = cowboy_req:reply(200, #{
          <<"content-type">> => <<"text/plain">>
  }, Res, Req0),
  {ok, Req, Opts}.

hash(Map) ->
  binary:bin_to_list(jiffy:encode(Map)).

atomJSON(Atom) -> list_to_binary(atom_to_list(Atom)).

% json output
o(Number) when is_number(Number) ->
  Number;
o(Atom) when is_atom(Atom) ->
  atomJSON(Atom);
o(Tuple) when is_tuple(Tuple) ->
  o(tuple_to_list(Tuple), []);
o(List) when is_list(List) ->
  case io_lib:printable_list(List) of
    true -> list_to_binary(List);
    false -> o(List, [])
  end;
o(Map) when is_map(Map) -> %expects atoms for internal map keys
  o(maps:keys(Map), Map, #{}).

o([], List) ->
  lists:reverse(List);
o([H | T], List) ->
  o(T, [o(H) | List]).

o([], _Map, Res) ->
  Res;
o([H | T], Map, Res) ->
  o(T, Map, maps:put(atomJSON(H), o(maps:get(H, Map)), Res)).

r(Map) ->
  hash(o(Map)).
  