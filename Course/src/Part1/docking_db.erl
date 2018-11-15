%%%-------------------------------------------------------------------
%%% @author Incerto
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2018 14:05
%%%-------------------------------------------------------------------
-module(docking_db).
-author("Incerto").

%% API
-export([new/3, write/2, read/2, delete/2]).


%% Docking station DB
new(Name, Total, Occupied) -> [].

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
