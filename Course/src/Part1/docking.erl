%%%-------------------------------------------------------------------
%%% @author 'Anon' (for assignment procedure reasons)
%%% @copyright None
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2018 11:20
%%%-------------------------------------------------------------------
-module(docking).
-author("Anon").

%% API
-export([start_link/3, release_moped/1, secure_moped/1, get_info/1]).

start_link(Total, Occupied, Name) ->
  {ok, Name}.

release_moped(Name) ->
  {ok, Name}.

secure_moped(Name) ->
  {ok, Name}.

get_info(Name) ->
  {ok, Name}.


