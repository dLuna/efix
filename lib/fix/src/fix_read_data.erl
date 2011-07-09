%% -*- erlang-indent-level: 2 -*-
%% @author Daniel Luna <daniel@lunas.se>
%% @copyright 2011 Daniel Luna

-module(fix_read_data).
-author('Daniel Luna <daniel@lunas.se>').
-export([fix40/2, fix41/2, fix42/2, fix43/2, fix44/2, fixt11/2]).
-compile({no_auto_import,[float/1]}).
-compile({no_auto_import,[length/1]}).

-define(SOH, 1).

%% Note that for all types that the base case of the empty string is
%% not necessary since all fields are terminated by SOH.  (And we
%% crash for bad data anyway.)

%% Char type in fix 4.0 looks a lot like a string in 4.2 and later
fix40(char, Data) -> string(Data);
fix40(int, Data) -> int(Data);
fix40(float, Data) -> float(Data);
fix40(time, Data) -> time(Data);
fix40(Type, Data) -> throw({not_yet_implemented_type, fix40, Type, Data}).

fix41(char, Data) -> string(Data);
fix41(int, Data) -> int(Data);
fix41(float, Data) -> float(Data);
fix41(time, Data) -> time(Data);
fix41(Type, Data) -> throw({not_yet_implemented_type, fix41, Type, Data}).

fix42(char, Data) -> char(Data);
fix42(string, Data) -> string(Data);
fix42(int, Data) -> int(Data);
fix42(boolean, Data) -> boolean(Data);
fix42(utctimestamp, Data) -> time(Data);
fix42(Type, Data) -> throw({not_yet_implemented_type, fix42, Type, Data}).

fix43(boolean, Data) -> boolean(Data);
fix43(char, Data) -> char(Data);
fix43(string, Data) -> string(Data);
fix43(length, Data) -> length(Data);
fix43(int, Data) -> int(Data);
fix43(seqnum, Data) -> seqnum(Data);
fix43(utctimestamp, Data) -> time(Data);
fix43(Type, Data) -> throw({not_yet_implemented_type, fix43, Type, Data}).

fix44(boolean, Data) -> boolean(Data);
fix44(char, Data) -> char(Data);
fix44(int, Data) -> int(Data);
fix44(string, Data) -> string(Data);
fix44(length, Data) -> length(Data);
fix44(seqnum, Data) -> seqnum(Data);
fix44(utctimestamp, Data) -> time(Data);
fix44(Type, Data) -> throw({not_yet_implemented_type, fix44, Type, Data}).

fixt11(int, Data) -> int(Data);
fixt11(length, Data) -> int(Data); %% length_(Data);
fixt11(tagnum, Data) -> int(Data); %% tagnum(Data);
fixt11(seqnum, Data) -> int(Data); %% seqnum(Data);
fixt11(numingroup, Data) -> int(Data); %% numingroup(Data);
fixt11(string, Data) -> string(Data);
fixt11(boolean, Data) -> boolean(Data);
fixt11(utctimestamp, Data) -> time(Data);
fixt11(Type, Data) -> throw({not_yet_implemented_type, fixt11, Type, Data}).

boolean([$Y, ?SOH | Rest]) -> {true, Rest};
boolean([$N, ?SOH | Rest]) -> {false, Rest}.

char([C, ?SOH | Rest]) -> {C, Rest}.

length(Data) -> posint(Data).

posint(Data) ->
  {Value, Rest} = int(Data),
  %% FIXME: Including zero or not?
  case Value >= 0 of
    true -> {Value, Rest};
    false -> throw({field_not_positive, Value})
  end.
  
seqnum(Data) -> posint(Data).

string(Data) -> string(Data, []).

string([?SOH | Rest], Acc) -> {lists:reverse(Acc), Rest};
string([C | Rest], Acc) -> string(Rest, [C | Acc]).

int(Data) ->
  {String, Rest} = string(Data),
  {list_to_integer(String), Rest}.

float(Data) ->
  {String, Rest} = string(Data),
  {list_to_float(String), Rest}.
  

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
