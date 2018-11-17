%%%-------------------------------------------------------------------
%%% @author Incerto (Pseudonym used for making purposes).
%%% @copyright (C) 2018, Incerto
%%% @doc
%%%
%%% @end
%%% Created : 4. Nov 2018 18:45
%%%-------------------------------------------------------------------
-module(docking).
-author("Incerto").

%% API
-export([start_link/3, idle/3, empty/3, full/3, secure/1, release/1, get_info/1, return_info/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Docking Station Prototype - Finite State Machine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc start a new docking station FSM.
start_link(Total, Occupied, Name) ->
  Pid = spawn(docking, idle, [Total, Occupied, Name]),
  register(Name, Pid),
  {ok, Pid}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM States
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc The Idle state.
idle(Total, Occupied, Name) ->
  receive
    secure ->
      if
        (Occupied + 1) == Total -> full(Total, Occupied + 1, Name);
        (Occupied + 1) < Total -> idle(Total, Occupied + 1, Name)
      end;
    release ->
      if
        (Occupied -1) == 0 -> empty(Total, Occupied - 1, Name);
        (Occupied -1) > 0 -> idle(Total, Occupied - 1, Name)
      end;
    get_info ->
      return_info(Total, Occupied, Name),
      idle(Total, Occupied, Name)
  end.

%% @doc The Empty state.
empty(Total, Occupied, Name) ->
  receive
    secure ->
      idle(Total, Occupied + 1, Name);
    release ->
      io:format('{error, empty}~n'),
      empty(Total, Occupied, Name);
    get_info ->
      return_info(Total, Occupied, Name),
      empty(Total, Occupied, Name)
  end.

%% @doc The Full state.
full(Total, Occupied, Name) ->
  receive
    secure ->
      io:format('{error, full}~n'),
      full(Total, Occupied, Name);
    release ->
      idle(Total, Occupied - 1, Name);
    get_info ->
      return_info(Total, Occupied, Name),
      full(Total, Occupied, Name)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FSM Events
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Secure a moped to the docking station (if possible).
secure(Name) -> Name ! secure, ok.

%% @doc Release a moped to the docking station (if possible).
release(Name) -> Name ! release, ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc The get_info API call.
get_info(Name) -> Name ! get_info.

%% @doc Format the output for the get_info api call.
return_info(Total, Occupied, Name) ->
  Free = Total - Occupied,
  io:format('{~p, [{total, ~p}, {occupied, ~p}, {free, ~p}]}~n', [Name, Total, Occupied, Free]).

