%%%-------------------------------------------------------------------
%%% @author Incerto
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2018 16:48
%%%-------------------------------------------------------------------
-module(simple).
-author("Incerto").

%% API
-export([server/1, client/1, start/0]).


server(State) ->
  receive
    {request, Return_PID} ->
      io:format("SERVER ~w: Client request recieved from ~w~n",
                [self(), Return_PID]),
      NewState = State + 1,
      Return_PID ! {hit_count, NewState},
      server(NewState)
  end.



client(Server_Address) ->
  Server_Address ! {request, self()},
  receive
    {hit_count, Number} ->
      io:format("CLIENT ~w: Hit Count was ~w~n", [self(), Number])
  end.

start() ->
  Server_PID = spawn(simple, server, [0]),
  spawn(simple, client, [Server_PID]).





