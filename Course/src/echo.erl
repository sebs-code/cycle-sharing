%%%-------------------------------------------------------------------
%%% @author Incerto
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2018 14:32
%%%-------------------------------------------------------------------
-module(echo).
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
      io:format("~p~n",Msg),
      loop();
    stop ->
      true
   end.

print(Term) ->
  echo ! {print, Term}.

stop() ->
  exit(echo, process_closed).



%% DB one
new() -> [].

destroy(Cb) -> ok.

write({Key, Element, Db) ->
  [{Key, Element}| Db].

read(Key, [{Key, Element}|_]) ->
  {ok, Element};
read(Key, [_|Db]) ->
  read(Key, Db);
read(_Key, []) ->
  {error, instance}.

delete(_, []) ->
  [];
delete(Key, [{Key, _}|Db]) ->
  Db;
delete(Key, [Head|Db]) ->
  [Head|delete(Key, Db)].


% Process Ring
start(ProcNum, _MsgNum, Msg) ->
  io:format("~w started~n", [ProcNum, self()]),
  Pid = spawn(ring, start, [ProcNum-1, self()]),
  io:format("~w connected to ~w~n", [ProcNum, self(), Pid]),
  Pid ! {msg, Msg},
  recieve {msg, Msg} -> ok end,
  io:format("~w:got ~w~n", [ProcNum, _Msg]).

start(1, Pid) ->
  io:format("~w started~n", [1, self()]),
  io:format("~w connected to ~w~n", [1, self(), Pid]).
  recieve {msg, Msg} -> ok end,
  io:format("~w:got ~w~n", [ProcNum, _Msg]).

start(ProcNum, FirstPid) ->
  io:format("~w started~n", [ProcNum, self()]),
  Pid = spawn(ring, start, [ProcNum-1, FirstPid]),
  io:format("~w connected to ~w~n", [ProcNum, self(), Pid]),
  recieve {msg, Msg} -> ok end,
  io:format("~w:got ~w~n", [ProcNum, _Msg]).