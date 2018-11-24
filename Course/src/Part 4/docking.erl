%%%-------------------------------------------------------------------
%%% @author Incerto (Pseudonym used for making purposes).
%%% @copyright (C) 2018, Incerto
%%% @doc
%%%
%%% @end
%%% Created : 4. Nov 2018 18:45
%%%-------------------------------------------------------------------
-module(docking).
-include_lib("stdlib/include/ms_transform.hrl").
-author("Incerto").

%% API
-export([start_link/3, idle/3, empty/3, full/3, secure/1, release/1]).
-export([get_info/1, return_info/3, find_moped/1, find_docking_point/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Docking Station Prototype - Finite State Machine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc start a new docking station FSM.
-spec start_link(Total :: non_neg_integer(), Occupied :: non_neg_integer(), Name :: atom()) -> state.
start_link(Total, Occupied, Name) ->

  %% Set exit message flag
  process_flag(trap_exit, true),

  if
    %% Check for logical errors in Total and Occupied inputs.
    Total < Occupied ->
      io:format('{error, occupied greater than total}~n');
    Total < 0 ->
      io:format('{error, total cannot be negative}~n');
    Occupied < 0 ->
      io:format('{error, occupied cannot be negetive}~n');

    %% Determine initial state
    Occupied == Total ->
      Pid = spawn(docking, full, [Total, Occupied, Name]),
      register(Name, Pid),
      {ok, Pid};
    Occupied == 0 ->
      Pid = spawn(docking, empty, [Total, Occupied, Name]),
      register(Name, Pid),
      {ok, Pid};
    (Occupied < Total) and (Occupied > 0) ->
      Pid = spawn(docking, idle, [Total, Occupied, Name]),
      register(Name, Pid),
      {ok, Pid}
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM States
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc The Idle state.
-spec idle(Total :: non_neg_integer(), Occupied :: non_neg_integer(), Name :: atom()) -> state.
idle(Total, Occupied, Name) ->
  receive
    secure ->
      if
        (Occupied + 1) == Total ->
          {dbprocess, db} ! {insert, self(), Name, Total, Occupied + 1},
          full(Total, Occupied + 1, Name);
        (Occupied + 1) < Total ->
          {dbprocess, db} ! {insert, self(), Name, Total, Occupied + 1},
          idle(Total, Occupied + 1, Name)
      end;
    release ->
      if
        (Occupied -1) == 0 ->
          {dbprocess, db} ! {insert, self(), Name, Total, Occupied - 1},
          empty(Total, Occupied - 1, Name);
        (Occupied -1) > 0 ->
          {dbprocess, db} ! {insert, self(), Name, Total, Occupied - 1},
          idle(Total, Occupied - 1, Name)
      end;
    get_info ->
      return_info(Total, Occupied, Name),
      idle(Total, Occupied, Name);
    find_moped ->
      return_find_moped(Name),
      idle(Total, Occupied, Name);
    find_docking_point->
      return_find_docking_point(Name),
      idle(Total, Occupied, Name)

  end.

%% @doc The Empty state.
-spec empty(Total :: non_neg_integer(), Occupied :: non_neg_integer(), Name :: atom()) -> state.
empty(Total, Occupied, Name) ->
  receive
    secure ->
      {dbprocess, db} ! {insert, self(), Name, Total, Occupied + 1},
      idle(Total, Occupied + 1, Name);
    release ->
      io:format('{error, empty}~n'),
      empty(Total, Occupied, Name);
    get_info ->
      return_info(Total, Occupied, Name),
      empty(Total, Occupied, Name);
    find_moped ->
      return_find_moped(Name),
      empty(Total, Occupied, Name);
    find_docking_point->
      return_find_docking_point(Name),
      empty(Total, Occupied, Name)
  end.

%% @doc The Full state.
-spec full(Total :: non_neg_integer(), Occupied :: non_neg_integer(), Name :: atom()) -> state.
full(Total, Occupied, Name) ->
  receive
    secure ->
      io:format('{error, full}~n'),
      full(Total, Occupied, Name);
    release ->
      {dbprocess, db} ! {insert, self(), Name, Total, Occupied -1},
      idle(Total, Occupied - 1, Name);
    get_info ->
      return_info(Total, Occupied, Name),
      full(Total, Occupied, Name);
    find_moped ->
      return_find_moped(Name),
      full(Total, Occupied, Name);
    find_docking_point->
      return_find_docking_point(Name),
      full(Total, Occupied, Name)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM Events
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Secure a moped to the docking station (if possible).
-spec secure(Name :: atom()) -> message.
secure(Name) -> Name ! secure, ok.

%% @doc Release a moped to the docking station (if possible).
-spec release(Name :: atom()) -> message.
release(Name) -> Name ! release, ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc The get_info API call.
-spec get_info(Name :: atom()) -> info.
get_info(Name) -> Name ! get_info.

%% @doc The find_moped API call.
-spec find_moped(Name :: atom()) -> info.
find_moped(Name) -> Name ! find_moped.

%% @doc The find_docking_point API call.
-spec find_docking_point(Name :: atom()) -> info.
find_docking_point(Name) -> Name ! find_docking_point.

%% @doc Format the output for the get_info api call.
-spec return_info(Total :: non_neg_integer(), Occupied :: non_neg_integer(), Name :: atom()) -> io.
return_info(Total, Occupied, Name) ->
  Free = Total - Occupied,
  io:format('{~p, [{total, ~p}, {occupied, ~p}, {free, ~p}]}~n', [Name, Total, Occupied, Free]).

%% @doc Format the output for the find_moped api call.
-spec return_find_moped(Name :: atom()) -> io.
return_find_moped(Name) ->
  {dbprocess, db} ! {find_moped, self(), Name},

  % Get response list from DB
  receive
    [_] ->
      MopedReplyQuery = [_],

      %% Format List
      MopedReplyList = [{lists:nth(1, X), [{total, lists:nth(2, X)}, {occupied, lists:nth(3, X)}, {free, (lists:nth(2, X) - lists:nth(3, X))}]} || X <- MopedReplyQuery],
      io:format('{ok, ~p}~n', [MopedReplyList])
  end.

%% @doc Format the output for the find_docking_point api call.
-spec return_find_docking_point(Name :: atom()) -> io.
return_find_docking_point(Name) ->
  {dbprocess, db} ! {find_dokcing_point, self(), Name},

  % Get response list from DB
  receive
    [_] ->
      DockingReplyQuery = [_],

      %% Format List
      DockingReplyList = [{lists:nth(1, X), [{total, lists:nth(2, X)}, {occupied, lists:nth(3, X)}, {free, (lists:nth(2, X) - lists:nth(3, X))}]} || X <- DockingReplyQuery],
      io:format('{ok, ~p}~n', [DockingReplyList])
  end.