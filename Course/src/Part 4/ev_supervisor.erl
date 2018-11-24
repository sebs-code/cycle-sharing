%%%-------------------------------------------------------------------
%%% @author Incerto (Pseudonym used for making purposes).
%%% @copyright (C) 2018, Incerto
%%% @doc
%%%
%%% @end
%%% Created : 4. Nov 2018 18:45
%%%-------------------------------------------------------------------
-module(ev_supervisor).
-author("Incerto").

%% API
-export([start_link/0, start_child/3, init/0, loop/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Docking Station Supervisor
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts the supervisor
-spec start_link() -> {ok, pid}.
start_link() ->
  Pid = spawn(ev_supervisor, init, []),
  register(db, Pid),
  {ok, Pid}.

%% @doc Start a new child (docking station) process
-spec start_child(Total :: non_neg_integer(), Occupied :: non_neg_integer(), Name :: atom()) -> {ok, pid}.
start_child(Total, Occupied, Name) ->
  Pid = spawn_link(docking, start_link, [Total, Occupied, Name]),
  {dbprocess, db} ! {insert, Pid, Name, Total, Occupied},
  {ok, Pid}.

%% @doc Supervisor callback
init() ->
  % Set loop
  process_flag(trap_exit, true),
  loop().

%% @doc Message loop
loop() ->
  receive
    {'EXIT', Pid, Reason} ->
      %% Get docking station state data
      {dbprocess, db} ! {state_data, Pid},

      receive
        {exit_data, Name, Occupied, Total} ->
        %% Delete old row with Pid ID
        {dbprocess, db} ! {delete, Pid},

        %% Start new child in same state
        start_child(Total, Occupied, Name)
      end,

      %% loop
      loop()
  end.



