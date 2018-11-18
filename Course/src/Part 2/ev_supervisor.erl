%%%-------------------------------------------------------------------
%%% @author Incerto (Pseudonym used for making purposes).
%%% @copyright (C) 2018, Incerto
%%% @doc
%%%
%%% @end
%%% Created : 4. Nov 2018 18:45
%%%-------------------------------------------------------------------
-module(ev_supervisor).
-behaviour(supervisor).
-author("Incerto").

%% API
-export([start_link/0, start_child/3, init/0, loop/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Docking Station Supervisor
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
  Pid = spawn(ev_supervisor, init, []),
  {ok, Pid}.

start_child(Total, Occupied, Name) ->
  Pid = spawn_link(docking, start_link, [Total, Occupied, Name]),
  ets:insert(docking_stations, {Pid, Name, Total, Occupied}),
  {ok, Pid}.


init() ->
  % ETS table to preserve state
  ets:new(docking_stations, [set, public, named_table]),

  % Set loop
  process_flag(trap_exit, true),
  loop().

loop() ->
  receive
    {'EXIT', Pid, Reason} ->
      %% Get docking station state data
      Name = ets:lookup_element(dockers, Pid, 2),
      Occupied = ets:lookup_element(dockers, Pid, 3),
      Total = ets:lookup_element(dockers, Pid, 4),

      %% Delete old row with Pid ID
      ets:delete(docking_stations, Pid),

      %% Start new child in same state
      start_child(Total, Occupied, Name),

      %% loop
      loop()
  end.



