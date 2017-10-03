-module(s).
-export([s/2]).
% Shortcuts.

% Send
s(Pid, Message) ->
  gen_server:call(Pid, Message).