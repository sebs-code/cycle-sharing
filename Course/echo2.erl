%%%-------------------------------------------------------------------
%%% @author Incerto
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2018 14:32
%%%-------------------------------------------------------------------
-module(echo2).
-author("Incerto").

%% API
-export([start/0, stop/0, print/1, loop/0]).

start() ->
  Pid = spawn(echo, loop, []),
  register(echo, Pid),
  ok.

loop() ->
  receive
    {print, Msg} ->
      io:format("~p,~n", [Msg]),
      print ! {self(), Msg},
      loop();
    stop ->
      true
   end.

print(Term) ->
  echo ! {self(), Term},
  receive
    {self(), Msg}->\
      io:format()
  end,
  ok.

stop() ->
  exit(echo, process_closed).