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

%% Char type in fix 4.0 looks a lot like a string in 5.0
fix40(char, Data) -> string(Data);
fix40(int, Data) -> int(Data);
fix40(time, Data) -> time(Data);
fix40(Type, Data) -> throw({not_yet_implemented_type, Type, Data}).

string(Data) -> string(Data, []).

string([?SOH | Rest], Acc) -> {lists:reverse(Acc), Rest};
string([C | Rest], Acc) -> string(Rest, [C | Acc]).

int(Data) ->
  {String, Rest} = string(Data),
  {list_to_integer(String), Rest}.

time(Data) ->
  {String, Rest} = string(Data),
  Time =
    case String of
      [Y1, Y2, Y3, Y4, M1, M2, D1, D2, $-,
       H1, H2, $:, Min1, Min2, $:, S1, S2] ->
        DateTime = {{list_to_integer([Y1, Y2, Y3, Y4]),
                     list_to_integer([M1, M2]),
                     list_to_integer([D1, D2])},
                    {list_to_integer([H1, H2]),
                     list_to_integer([Min1, Min2]),
                     list_to_integer([S1, S2])}},
        calendar:datetime_to_gregorian_seconds(DateTime) * 1000;
      %% The following is only a valid date in FIX 5.0 and later, but
      %% it's allowed nevertheless.
      [Y1, Y2, Y3, Y4, M1, M2, D1, D2, $-,
       H1, H2, $:, Min1, Min2, $:, S1, S2,
       $., Ms1, Ms2, Ms3] ->
        DateTime = {{list_to_integer([Y1, Y2, Y3, Y4]),
                     list_to_integer([M1, M2]),
                     list_to_integer([D1, D2])},
                    {list_to_integer([H1, H2]),
                     list_to_integer([Min1, Min2]),
                     list_to_integer([S1, S2])}},
        calendar:datetime_to_gregorian_seconds(DateTime) * 1000 +
          list_to_integer([Ms1, Ms2, Ms3])
    end,
  {Time, Rest}.
