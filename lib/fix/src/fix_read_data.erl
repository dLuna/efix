%% -*- erlang-indent-level: 2 -*-
%% @author Daniel Luna <daniel@lunas.se>
%% @copyright 2011 Daniel Luna

-module(fix_read_data).
-author('Daniel Luna <daniel@lunas.se>').
-export([fix40/2]).

-define(SOH, 1).

%% Note that for all types that the base case of the empty string is
%% not necessary since all fields are terminated by SOH.  (And we
%% crash for bad data anyway.)

fix40(char, Data) ->
  %% Char type in fix 4.0 looks a lot like a string in 5.0
  string(Data, []);
fix40(Type, Data) ->
  throw({not_yet_implemented_type, Type, Data}).
 
string([?SOH | Rest], Acc) -> {lists:reverse(Acc), Rest};
string([C | Rest], Acc) -> string(Rest, [C | Acc]).
