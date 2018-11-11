%%%-------------------------------------------------------------------
%%% @author Incerto
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2018 14:05
%%%-------------------------------------------------------------------
-module(db).
-author("Incerto").

%% API
-export([new/0, destroy/1, write/2, read/2, delete/2]).


%% DB one
new() -> [].

destroy(Db) -> ok.

write({Key, Element}, Db) ->
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
