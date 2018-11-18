%%%-------------------------------------------------------------------
%%% @author Incerto
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Nov 2018 09:17
%%%-------------------------------------------------------------------
-module(test).
-author("Incerto").

%% API
-export([start_link/1, init/1]).

start_link(Name) ->
  Pid = spawn_link(test, init, [Name]),
  register(Name, Pid),
  {ok, Pid}.

init(Name) ->
  io:format('~p started~n', [Name]),
  loop().

loop() -> receive X -> X end.


