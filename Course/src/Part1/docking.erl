%%%-------------------------------------------------------------------
%%% @author 'Anon' (for assignment procedure reasons)
%%% @copyright None
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2018 11:20
%%%-------------------------------------------------------------------
-module(docking).
-author("Anon").

%% API
-export([start_link/3, release_moped/1, secure_moped/1, get_info/1, init/1, loop/1]).

%% Docking Station API
start_link(Total, Occupied, Name) ->
  Docking_Db = docking_db:new(Name, Total, Occupied),
  init(Docking_Db).

release_moped(Name) ->
  {ok, Name}.

secure_moped(Name) ->
  {ok, Name}.

get_info(Name) ->
  docking ! {read, Name}.

%% Helper functions
init(Docking_Db) ->
  Pid = spawn(docking, loop, [Docking_Db]),
  register(docking, Pid),
  {ok, Pid}.

loop(Docking_Db) ->
  receive
    new ->
      docking_db:new(),
      loop(Docking_Db);
    {write, Key, Element} ->
      docking_db:write({Key, Element}, Docking_Db),
      loop(Docking_Db);
    {delete, Key} ->
      docking_db:delete(Key, [{Key, Key}|Docking_Db]),
      loop(Docking_Db);
    {read, Key} ->
      docking_db:read(Key, Docking_Db);
    stop ->
      true
  end.