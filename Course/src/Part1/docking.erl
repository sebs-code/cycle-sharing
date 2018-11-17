%%%-------------------------------------------------------------------
%%% @author Incerto (Pseudonym used for marking purposes)
%%% @copyright (C) 2018, Incerto
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2018 11:20
%%%-------------------------------------------------------------------
-module(docking).
-author("Incerto").

%% API
-export([start_link/3, release_moped/1, secure_moped/1, get_info/1]).

%% Helper functions
-export([init/1, loop/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Docking Station API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc start a new docking station.
start_link(Total, Occupied, Name) ->
  Docking_Db = docking_db:new(Name, Total, Occupied),
  init(Docking_Db).

%% @doc release a moped from a docking station.
release_moped(Name) ->
  docking ! {release, Name}.

%% @doc secure a moped to a docking station.
secure_moped(Name) ->
  docking ! {secure, Name}.

%% @doc release a moped from a docking station.
get_info(Name) ->
  docking ! {read, Name}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc initialised the 'database' and messaging loop for FSM changes.
init(Docking_Db) ->
  Pid = spawn(docking, loop, [Docking_Db]),
  register(docking, Pid),
  {ok, Pid}.

%% @doc loop pattern for message parsing.
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