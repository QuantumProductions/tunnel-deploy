-module(board).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

recentTile_test() ->
  Board = default(),
  {ok, Board2, 0, false} = place(take, Board, x, {1, 2}),
  {recent, x, none} = tile(Board2, {1,2}),
  {ok, Board3, 0, false} = place(take, Board2, o, {4, 5}),
  {ok, Board4, 0, false} = place(take, Board3, o, {3, 5}),
  {ok, Board5, 0, false} = place(take, Board4, o, {2, 5}),
  {ok, Board6, 0, false} = place(take, Board5, o, {1, 5}),
  {ok, Board7, 0, false} = place(take, Board6, o, {1, 4}),
  {ok, Board8, 0, false} = place(take, Board7, o, {1, 3}),
  {ok, _, 0, true}  = place(take, Board8, o, {1, 2}),

  ResolveBoard = resolveRecent(Board2, x),
  {empty, x, none} = tile(ResolveBoard, {1, 2}).

takableFromSpawn_test() ->
  Board = default(),
  true = takable(Board, x, {2, 1}),
  true = takable(Board, x, {1, 2}),
  false = takable(Board, x, {2, 2}),
  false = takable(Board, x, {5, 5}),
  true = takable(Board, o, {4, 5}),
  true = takable(Board, o, {5, 4}).

place(take, Board, X, Position) ->
  Contiguous = contiguous(Board, X),
  case adjacent(Contiguous, Position) of
    true -> place(take_reached, Board, X, Position);
    false -> {error, unreachable, Board}
  end;
place(take_reached, Board, X, Position) ->
  case takable(Board, X, Position) of
    true -> take(Board, X, Position);
    false -> {error, blocked, Board}
  end;
place(WallDirection, Board, X, Position) ->
  Contiguous = contiguous(Board, X),
  case lists:member(Position, Contiguous) of
   true -> place(wallable, WallDirection, Board, X, Position);
   false -> {error, wall_unreachable, Board}
  end.
place(wallable, WallDirection, Board, X, Position) ->
  case wallable(Board, X, Position) of
    true -> wall(Board, WallDirection, Position);
    false -> {error, unwallable, Board}
  end.

takeStatus(Board, x) ->
  case length(contiguous(Board, x)) of
    0 -> owin;
    _ -> playing
  end;
takeStatus(Board, o) ->
  case length(contiguous(Board, o)) of
    0 -> xwin;
    _ -> playing
  end.

recent({recent, _, _}) -> true;
recent({ridge_recent, _, _}) -> true;
recent(_) -> false.

resolveRecent(Board, Team) ->
  resolveRecent(files, Board, Team, length(Board), []).
resolveRecent(files, _Board, _Team, 0, Resolved) ->
  Resolved;
resolveRecent(files, Board, Team, File, Resolved) ->
  resolveRecent(files, Board, Team, File - 1 , [resolveRecent(rank, Team, lists:nth(File, Board)) | Resolved]);
resolveRecent(rank, _Team, _Rank, 0, Resolved) ->
  Resolved;
resolveRecent(rank, Team, Rank, Column, Resolved) ->
  resolveRecent(rank, Team, Rank, Column - 1, [resolveRecent(tile, Team, lists:nth(Column, Rank)) | Resolved]).

resolveRecent(rank, Team, Rank) ->
  resolveRecent(rank, Team, Rank, length(Rank), []);
resolveRecent(tile, ResolveTeam, {Status, OtherTeam, Wall}) when ResolveTeam /= OtherTeam ->
  {Status, OtherTeam, Wall};
resolveRecent(tile, Team, {recent, Team, Wall}) ->
  {empty, Team, Wall};
resolveRecent(tile, Team, {ridge_recent, Team, Wall}) ->
  {ridge, Team, Wall};
resolveRecent(tile, Team, {Other, Team, Wall}) ->
  {Other, Team, Wall}.

sliceStatus(ridge_recent) ->
  ridge;
sliceStatus(recent) ->
  empty;
sliceStatus(Any) ->
  Any.
  
sliceTile({Status, _OldTeam, _OldWall}, Taker) ->
  {sliceStatus(Status), Taker, none}.

slicedBoard(Board, [{Rank, File} | TSlicedCoordinates], Taker) ->
  slicedBoard(replace(Board, sliceTile(tile(Board, {Rank, File}), Taker), Rank, File),
    TSlicedCoordinates, Taker);
slicedBoard(Board, [], _) ->
  Board.

take(Board, X, {Rank, File}) ->
  % First update the board
  TileTaken = tile(Board, {Rank, File}),
  TakingRecent = recent(TileTaken),
  Board2 = replace(Board, captureTile(TileTaken, X), Rank, File),
  Opponent = otherPlayer(X),
  % Find all defender tiles
  DefenderTiles = playerTiles(Board2, Opponent),
  % Find all tiles contiguous to defender's spawn
  ContiguousTiles = contiguous(Board2, Opponent),
  % Subtract contiguous from all
  Remainder = DefenderTiles -- ContiguousTiles,
  % Recursively slice remainder
  Sliced = slice(Board2, Remainder, Opponent),
  Board3 = slicedBoard(Board2, Remainder, X),
  Status = takeStatus(Board2, Opponent),
  {Status, Board3, Sliced, TakingRecent}.

adjacent([], _) ->
  false;
adjacent([{PRank, PFile} | T], {Rank, File}) ->
  case  ((PFile) =:= (File - 1) andalso PRank =:= Rank) orelse
        ((PFile) =:= (File + 1) andalso PRank =:= Rank) orelse
        ((PRank) =:= (Rank - 1) andalso PFile =:= File) orelse
        ((PRank) =:= (Rank + 1) andalso PFile =:= File) of
    true -> true;
    false -> adjacent(T, {Rank, File})
  end.

takable(Board, X, {Rank, File}) ->
  TargetTile = tile(Board, {Rank, File}),
  takable2([{tile(Board, {Rank - 1, File}), west},
   {tile(Board, {Rank + 1, File}), east},
   {tile(Board, {Rank, File - 1}), north},
   {tile(Board, {Rank, File + 1}), south}
   ], X, TargetTile).

takable2([], _, _) ->
  false;
takable2( [{null, _WallDirection} | T], X, TargetTile) ->
  takable2(T, X, TargetTile);
takable2([{{_, null, _}, _} | T], X, TargetTile) ->
  takable2(T, X, TargetTile);
takable2([{{_, O, _}, _} | T], X, TargetTile) when X =/= O ->
  takable2(T, X, TargetTile);
takable2([{{_,X,_}, WallDirection} | _T], X, {_,_,OtherWallDirection}) when WallDirection =/= OtherWallDirection ->
  true;
takable2([_H | T], X, TargetTile) ->
  takable2(T, X, TargetTile).

slice(Board, Remainder, X) ->
  slice(Board, Remainder, X, 0).
slice(_, [], _X, Slices) ->
  Slices;
slice(Board, [FirstTile | T], X, Slices) ->
  Contiguous = contiguous(Board, X, FirstTile),
  slice(Board, [FirstTile | T] -- Contiguous, X, Slices + 1).

contiguous(Board, X) ->
  contiguous(valid, Board, X, spawnTile(X), []).
contiguous(Board, X, Position) ->
  contiguous(valid, Board, X, Position, []).
contiguous(valid, Board, X, Position, Visited) ->
  Tile = tile(Board, Position),
  case Tile of 
    null -> Visited;
    {_, X, _Wall} -> contiguous(member, Board, X, Position, Visited);
    _ -> Visited
  end;
contiguous(member, Board, X, Position, Visited) ->
  case lists:member(Position, Visited) of
    true -> Visited;
    false -> contiguous(add, Board, X, Position, Visited)
  end;
contiguous(add, Board, X, {Rank, File}, Visited) ->
  VisitedAdded  = [{Rank, File} | Visited],
  VisitedLeft   = contiguous(valid, Board, X, {Rank - 1, File}, VisitedAdded),
  VisitedUp     = contiguous(valid, Board, X, {Rank, File - 1}, VisitedLeft),
  VisitedRight  = contiguous(valid, Board, X, {Rank + 1, File}, VisitedUp),
  contiguous(valid, Board, X, {Rank, File + 1}, VisitedRight).

captureTile({ridge_recent, _, _}, X) -> {ridge_recent, X, none};
captureTile({ridge, _, _}, X) -> {ridge_recent, X, none};
captureTile({_, _, _}, X) -> {recent, X, none}.

playerTiles(Board, X) ->
  playerTiles(Board, X, length(Board), []).
playerTiles(_Board, _X, 0, Positions) ->
  Positions;
playerTiles(Board, X, File, Positions) ->
  playerTiles(Board, X, File - 1, Positions ++ playerRankTiles(lists:nth(File, Board), X, File)).

playerRankTiles(Row, X, File) ->
  playerRankTiles(Row, X, length(Row), File, []).
playerRankTiles(_Row, _X, 0, _File, Positions) ->
  Positions;
playerRankTiles(Row, X, Rank, File, Positions) ->
  case lists:nth(Rank, Row) of
    {_,X,_} -> playerRankTiles(Row, X, Rank - 1, File, [{Rank, File} | Positions]);
    _ -> playerRankTiles(Row, X, Rank - 1, File, Positions)
  end.

wallable(Board, X, {Rank, File}) ->
  OtherPlayer = otherPlayer(X),
  case tile(Board, {Rank, File}) of
    {_, OtherPlayer, _Wall} -> false;
    {ridge, _, _} -> false;
    {spawn, _, _} -> false;
    {recent, X, _} -> true;
    {empty, X, _} -> true;
    {ridge_recent, X, _} -> false;
    _ -> false
  end.

wall(Board, WallDirection, {Rank, File}) ->
  {Status, Player, _ExistingWall} = tile(Board, {Rank, File}),
  Board2 = replace(Board, {Status, Player, WallDirection}, Rank, File),
  {playing, Board2, 0, false}.

otherPlayer(x) -> o;
otherPlayer(o) -> x.

tile(_Board, {Rank, _}) when Rank > 5; Rank < 1 -> null;
tile(_Board, {_, File}) when File > 5; File < 1 -> null;
tile(Board, {Rank, File}) ->
  lists:nth(Rank, lists:nth(File, Board)).

replace(Board, Tile, Rank, File) ->
  ExistingFile = lists:nth(File, Board),
  NewFile = lists:sublist(ExistingFile, Rank - 1) ++ [Tile] ++ lists:nthtail(Rank, ExistingFile),
  lists:sublist(Board, File - 1) ++ [NewFile] ++ lists:nthtail(File, Board).
  
default() ->
  default(5).
default(Files) ->
  default([], Files).
default(Files, Total) ->
  Columns = length(Files),
  case Columns of
    Total -> Files;
    _ -> default(lists:append(Files, [defaultRank(Total, Columns + 1)]), Total)
  end.
defaultRank(Tiles, File) ->
  defaultRank([], Tiles, File).
defaultRank(Rank, Total, File) ->
  Row = length(Rank),
  case Row of
    Total -> Rank;
    _ -> defaultRank(lists:append(Rank, [startingTile(Row + 1, File)]), Total, File)
  end.

spawnTile(x) -> {1, 1};
spawnTile(o) -> {5, 5}.

startingTile(1, 1) -> {spawn, x, none};
startingTile(5, 5) -> {spawn, o, none};

% startingTile(1, 2) -> {recent, x, none};
% startingTile(2, 2) -> {recent, o, south};
% startingTile(1, 3) -> {recent, x, east};
% startingTile(2, 3) -> {ridge_recent, x, none};
% startingTile(5, 1) -> {recent, x, none};
% startingTile(5, 2) -> {recent, o, none};

startingTile(2, 4) -> {ridge, null, none};
startingTile(3, 3) -> {ridge, null, none};
startingTile(4, 2) -> {ridge, null, none};
startingTile(_, _) -> {empty, null, none}.

validPlayer(o) -> true;
validPlayer(x) -> true;
validPlayer(_) -> false.

validInput(Action, Player, Position) ->
  case lists:member(Action, [take, east, north, south, west]) of
    true -> validInput(Player, Position);
    false -> false
  end.
validInput(Player, Position) ->
  case validPlayer(Player) of
    true -> validInput(Position);
    false -> false
  end.
validInput({Rank, File}) ->
  Rank >= 1 andalso Rank =< 5 andalso File >= 1 andalso File =< 5;
validInput(_) ->
  false.

iterate(Board, CurrentPlayer, {_Action, Player, _Position}) when Player =/= CurrentPlayer ->
  {reply, {error, out_of_order, Board}, Board};
iterate(Board, _CurrentPlayer, {Action, Player, Position}) ->
  Reply = place(Action, Board, Player, Position),
  case Reply of
    {_Status, ResultBoard, _Slices, _RecentTaken} -> {reply, Reply, ResultBoard};
    ErrorReply -> {reply, ErrorReply, Board}
  end.

handle_call(info, _From, State) ->
  {reply, State, State};
handle_call({place, CurrentPlayer, {Action, Player, Position}}, _From, Board) ->
  case validInput(Action, Player, Position) andalso validPlayer(CurrentPlayer) of
    true -> iterate(Board, CurrentPlayer, {Action, Player, Position});
    false -> {reply, {error, invalid_input, Board}, Board}
  end;
handle_call({cycle, Team}, _From, Board) ->
  RecentBoard = resolveRecent(Board, Team),
  {reply, {ok, RecentBoard}, RecentBoard};
handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};
handle_call(_, _, Board) ->
  {reply, {error, unrecognized, Board}, Board}.

init([]) -> 
  {ok, default()}.

go() ->
  gen_server:start_link(?MODULE, [], []).

terminate(normal, State) ->
    io:format("Board.~p~n", [State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 

handle_cast(_, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.