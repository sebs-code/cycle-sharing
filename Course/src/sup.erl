%%%-------------------------------------------------------------------
%%% @author Incerto
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Nov 2018 09:13
%%%-------------------------------------------------------------------
-module(sup).
-behaviour(supervisor).
-author("Incerto").

%% API
-export([start_link/0, init/1, loop/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  {ok, {{simple_one_for_one, 2, 3600},
         [{test, {test, start_link, []},
           permanent, 2000, worker, [test]}]}},
  loop().

loop() ->
  receive

  end


