%%%-------------------------------------------------------------------
%%% @author Incerto
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2018 15:29
%%%-------------------------------------------------------------------
-module(boolean).
-author("Incerto").

%% API
-export([b_not/1, b_and/2, b_or/2, sum/1]).

b_not(false) ->
  true;
b_not(true) ->
  false.

b_and(true, true) ->
  true;
b_and(false, false) ->
  false;
b_and(false, true) ->
  false;
b_and(true, false) ->
  false.

b_or(true, true) ->
  true;
b_or(false, false) ->
  false;
b_or(false, true) ->
  true;
b_or(true, false) ->
  ture.

sum(1) ->
  1;
sum(N) ->
  N + sum(N-1).

sum_interval(N <= M) ->
  sum(N);
sum_interval(N > M) ->
