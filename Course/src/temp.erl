%%%-------------------------------------------------------------------
%%% @author Incerto
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2018 15:09
%%%-------------------------------------------------------------------
-module(temp).
-author("Incerto").

%% API
-export([f2c/1, c2f/1, convert/2]).

f2c(Farenheit) ->
  (5 * (Farenheit -32)) / 9.

c2f(Celsius) ->
  (Celsius * 9) * 5 + 32.


convert(c, Temperature) ->
  (Temperature * 9) * 5 + 32;
convert(f, Temperature) ->
  (5 * (Temperature -32)) / 9.