%%%-------------------------------------------------------------------
%%% @author Incerto (Pseudonym used for marking purposes)
%%% @copyright (C) 2018, Incerto
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2018 11:20
%%%-------------------------------------------------------------------
-module(docking_db).
-author("Incerto").

-export([new/3, get_info/3]).
-export_type([db/0]).
-type db() :: list().

%% @doc Create a new docking station database
new(Total, Occupied, Name) ->
  [{Name, [{total, Total}, {occupied, Occupied}, {free, Total - Occupied}]}].

%% @doc Search for the docking station found in the 'database', and return associated data.
get_info(Pid, Name, [{Name, Element}|_Db]) ->
  reply(Pid, Element);
get_info(Pid, Name, [_Tuple|Db]) ->
  get_info(Pid, Name, Db);
get_info(Pid, _Key, []) ->
  reply(Pid, {error, instance}).



%%get_info(Name, [{Total, Occupied} | _Db]) ->
%%  {ok, Total};
%%get_info(Name, [_Tuple | Db]) ->
%%  get_info(Name, Db);
%%get_info(_Name, []) ->
%%  {error, instance}.

reply(Pid, Message) ->
  Pid ! {reply, Message}.