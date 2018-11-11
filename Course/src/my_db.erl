%%%-------------------------------------------------------------------
%%% @author Incerto
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
 %%%
%%% @end
%%% Created : 10. Oct 2018 14:00
%%%-------------------------------------------------------------------
-module(my_db).
-author("Incerto").

%% API
-export([start/0, init/1, loop/1, stop/0, write/2, delete/1, read/1, match/1]).

% DB Functions / API
start() ->
  My_Db = db:new(),
  init(My_Db).

init(My_Db) ->
  Pid = spawn(my_db, loop, [My_Db]),
  register(my_db, Pid),
  ok.

loop(My_Db) ->
  receive
    new ->
      db:new(),
      loop(My_Db);
    {write, Key, Element} ->
      db:write({Key, Element}, My_Db),
      loop(My_Db);
    {delete, Key} ->
      db:delete(Key, [{Key, Key}|My_Db]),
      loop(My_Db);
    {read, Key} ->
      db:read(Key, My_Db),
      loop(My_Db);
    {match, Element} ->
      loop(My_Db);
    stop ->
      true
  end.


stop() ->
  my_db ! stop,
  ok.

write(Key, Element) ->
  my_db ! {write, Key, Element}.

delete(Key) ->
  my_db ! {delete, Key}.

read(Key) ->
  my_db ! {read, Key},
%%  receive
%%    {ok, Element} -> Element
%%  end.

match(Element) ->
  my_db ! {match, Element}.

