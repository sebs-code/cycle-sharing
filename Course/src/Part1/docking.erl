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
-export([start_link/3, get_info/1]).

%% Helper functions
-export([init/1, loop/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Docking Station API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc start a new docking station.
start_link(Total, Occupied, Name) ->
  Docking_Db = docking_db:new(Total, Occupied, Name),
  init(Docking_Db).

%% @doc release a moped from a docking station.
release_moped(Name) ->
  docking ! {release, Name}.

%% @doc secure a moped to a docking station.
secure_moped(Name) ->
  docking ! {secure, Name}.

%% @doc release a moped from a docking station.
get_info(Name) ->
  docking ! {get_info, Name}.



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
    {get_info, Name} ->
      docking_db:get_info(self(), Name, Docking_Db),
      loop(Docking_Db);
    {reply, Message} ->
      io:format('~w~n', [Message]),
      loop(Docking_Db);
    stop ->
      true
  end.