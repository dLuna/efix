%% -*- erlang-indent-level: 2 -*-
%% @author Daniel Luna <daniel@lunas.se>
%% @copyright 2011 Daniel Luna

-module(fix).
-author('Daniel Luna <daniel@lunas.se>').
-export([decode/1, start/0]).

decode(String) ->
  case String of
    "8=FIX.4.0" ++ [1] ++ _ -> fix40:decode(String);
    "8=FIX.4.1" ++ [1] ++ _ -> fix41:decode(String);
    "8=FIX.4.2" ++ [1] ++ _ -> fix42:decode(String);
    "8=FIX.4.3" ++ [1] ++ _ -> fix43:decode(String);
    "8=FIX.4.4" ++ [1] ++ _ -> fix44:decode(String);
    "8=FIX.5.0" ++ [1] ++ _ -> fix50:decode(String);
    "8=FIX.5.0SP1" ++ [1] ++ _ -> fix50sp1:decode(String);
    "8=FIX.5.0SP2" ++ [1] ++ _ -> fix50sp2:decode(String);
    "8=FIXT.1.1" ++ [1] ++ _ -> fixt11:decode(String);
    "8=" ++ _ -> throw(unsupported_fix_version);
    _ -> throw(bad_data)
  end.

start() ->
  ok.
