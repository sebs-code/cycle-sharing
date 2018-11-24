%%%-------------------------------------------------------------------
%%% @author Incerto (Pseudonym used for making purposes).
%%% @copyright (C) 2018, Incerto
%%% @doc
%%%
%%% @end
%%% Created : 4. Nov 2018 18:45
%%%-------------------------------------------------------------------
-module(ev_db).
-author("Incerto").

%% API
-export([start_db/0]).


%% @doc start a new docking station FSM.
start_db() ->
  Pid = spawn(ev_db, init, []),
  {ok, Pid}.

init() ->
  % ETS table to preserve state
  ets:new(docking_stations, [set, public, named_table]),

  % Set loop
  loop().

loop() ->
  receive
    {insert, Pid, Name, Total, Occupied} ->
      ets:insert(docking_stations, {Pid, Name, Total, Occupied}),
      loop();

    {delete, Pid} ->
      ets:delete(docking_stations, Pid),
      loop();

    {state_data, Pid} ->
      Name = ets:lookup_element(dockers, Pid, 2),
      Occupied = ets:lookup_element(dockers, Pid, 3),
      Total = ets:lookup_element(dockers, Pid, 4),
      % Return data to process
      Pid ! {exit_data, Name, Occupied, Total};

    {find_moped, Pid, Name} ->
      % Get docking_stations with free mopeds
      MopedReplyQuery = [[DockName, Total, Occupied, (Total - Occupied)] || [DockName, Total, Occupied] <-
        ets:match(docking_stations, {'_', '$1', '$2', '$3'}), Occupied > 0, DockName =/= Name],

      % Send data back to requesting process
      Pid ! MopedReplyQuery,
      loop();

    {find_docking_point, Pid, Name} ->
      % Get docking stations with free docking points
      DockingReplyQuery = [[DockName, Total, Occupied, (Total - Occupied)] || [DockName, Total, Occupied] <-
        ets:match(docking_stations, {'_', '$1', '$2', '$3'}), Occupied < Total, DockName =/= Name],

      % Send data back to requesting process
      Pid ! DockingReplyQuery,
      loop()
  end.