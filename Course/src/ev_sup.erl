%%%-------------------------------------------------------------------
%%% @author Incerto
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Nov 2018 13:23
%%%-------------------------------------------------------------------
-module(ev_sup).
-author("Incerto").

%% Client Functions
-export([start_link/1, start_child/4]).

%% Internal Exports
-export([init/0]).

%% @doc Starts the supervisor
-spec start_link(atom()) -> {ok, pid()}.
start_link(Name) ->
  Pid = spawn(sup, init, []),
  register(Name, Pid),
  {ok, Pid}.


%% @doc Given a module, function and arguments, will start a child
%% and monitor it. If it terminates abnormally, the child is
%% restarted.
-spec start_child(atom(), non_neg_integer(), non_neg_integer(), atom()) -> {ok, pid()}.
start_child(Id, Total, Occupied, Name) ->
  Id ! {start_child, self(), Module, Function, Args},
  receive
    {ok, Pid} -> {ok, Pid}
  end.

%% @doc Initialises the supervisor state
-spec init() -> ok.
init() ->
  process_flag(trap_exit, true),
  loop([]).


%%% loop([child()]) -> ok.
%%% child() = {pid(), restart_count(), mod(), func(), [args()]}.
%%% restart_count() = integer(). number of times the child has restarted
%%% mod() = atom(). the module where the spawned function is located
%%% func() = atom(). the function spawned
%%% args() = term(). the arguments passed to the function
%%% The supervisor loop which handles the incoming client requests
%%% and EXIT signals from supervised children.=
-spec loop() -> ok.
loop(Children) ->
  receive
    {start_child, ClientPid, Mod, Func, Args} ->
      Pid = spawn_link(Mod, Func, Args),
      ClientPid ! {ok, Pid},
      loop([{Pid, 1, Mod, Func, Args}|Children]);
    {'EXIT', Pid, normal} ->
      NewChildren = lists:keydelete(Pid, 1, Children),
      loop(NewChildren);
    {'EXIT', Pid, Reason} ->
      NewChildren = lists:keydelete(Pid, 1, Children),
      {value, Child} = lists:keysearch(Pid, 1, Children),
      {Pid, Count, Mod, Func, Args} = Child,
      error_message(Pid, Count, Reason, Mod, Func, Args),
      NewPid = spawn_link(Mod, Func, Args),
      loop([{NewPid, Count + 1, Mod, Func, Args}|NewChildren]);
  end.


%%% Prints an error message for the child which died.
-spec error_message(pid(), non_neg_integer(), term(), atom(), atom(), [term()])
      -> ok.
error_message(Pid, Count, Reason, Mod, Func, Args) ->
  io:format("~50c~n",[$-]),
  io:format("Error: Process ~p Terminated ~p time(s)~n",[Pid, Count]),
  io:format("       Reason for termination:~p~n",[Reason]),
  io:format("       Restarting with ~p:~p/~p~n",[Mod,Func,length(Args)]),
  io:format("~50c~n",[$-]).