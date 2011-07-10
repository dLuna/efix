%% -*- erlang-indent-level: 2 -*-
%% @author Daniel Luna <daniel@lunas.se>
%% @copyright 2011 Daniel Luna

-module(fix_test).
-author('Daniel Luna <daniel@lunas.se>').
-export([run/1, test/1]).

run(RootDir) ->
  {ok, Dirs} = file:list_dir(RootDir),
  Files = 
    lists:flatmap(fun(Dir) ->
                      D = RootDir ++ Dir,
                      {ok, Files} = file:list_dir(D),
                      [D ++ [$/ | File] || File <- Files]
                  end,
                  Dirs),
  Results = [test(File) || File <- Files],
  ok = file:write_file("full_test_results.txt",
                       io_lib:format("~p", [Results])),
  [io:format("~s: ~p~n",
             [File,
              length(lists:filter(fun(Res) -> Res =:= ok end, Ress))
              * 100 div length(Ress)]) || {File, Ress} <- Results].

test(Filename) ->
  {ok, Dev} = file:open(Filename, read),
  {lists:flatten(Filename), by_line(file:read_line(Dev), Dev)}.

by_line({ok, "I" ++ Line}, Dev) ->
  [parse_result(Line) | by_line(file:read_line(Dev), Dev)];
by_line({ok, "E" ++ Line}, Dev) ->
  [parse_result(Line) | by_line(file:read_line(Dev), Dev)];
by_line({ok, _}, Dev) -> by_line(file:read_line(Dev), Dev);
by_line(eof, Dev) -> file:close(Dev), [].

parse_result(Line0) ->
  Line1 =
    case lists:reverse(Line0) of
      [$\n | Rest ] -> lists:reverse(Rest);
      _ -> Line0 %% Only possible for the last line in file.
    end,
  Line2 = add_ts(Line1),
  Line3 = clean(Line2),
  Line = set_checksum(Line3),
  try
    fix:decode(Line),
    ok
  catch
    Type:Reason ->
      {fail, {Type, Reason, pp(Line)}, erlang:get_stacktrace()}
  end.

add_ts("<TIME>" ++ Rest) ->
  timestamp() ++ add_ts(Rest);
add_ts("<TIME+" ++ Rest0) ->
  {Add, Rest} = get_nums(Rest0, []),
  timestamp(Add) ++ add_ts(Rest);
add_ts("<TIME-" ++ Rest0) ->
  {Remove, Rest} = get_nums(Rest0, []),
  timestamp(-Remove) ++ add_ts(Rest);
add_ts("00000000-" ++ Rest) -> "20110707-" ++ add_ts(Rest);
add_ts([C | Rest]) -> [C | add_ts(Rest)];
add_ts([]) -> [].

get_nums([$> | Rest], Acc) -> {list_to_integer(lists:reverse(Acc)), Rest};
get_nums([C | Rest], Acc) -> get_nums(Rest, [C | Acc]).

timestamp() ->
  timestamp(0).

timestamp(Diff) ->
  {{YYYY, MM, DD}, {Hour, Min, Sec}} =
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(erlang:localtime()) + Diff),
  lists:flatten(io_lib:format("~4..0w~2..0w~2..0w-~2..0w:~2..0w:~2..0w",
                              [YYYY, MM, DD, Hour, Min, Sec])).

pp([]) -> [];
pp([1 | Rest]) -> "SOH" ++ pp(Rest);
pp([C | Rest]) -> [C | pp(Rest)].

clean("1," ++ Rest) -> Rest;
clean("2," ++ Rest) -> Rest;
clean(Rest) -> Rest.

set_checksum(Line) ->
  case lists:reverse(Line) of
    [1, $0, $=, $0, $1 | Enil] ->
      CS = integer_to_list(lists:sum(Enil) rem 256),
      CSString = add_zeroes(CS),
      lists:reverse(Enil) ++ "10=" ++ CSString ++ [1];
    [1, _, _, _, $=, $0, $1 | _] ->
      Line; % A proper checksum already exists
    [1, _, _, $=, $0, $1 | _] ->
      Line; % A badly formatted checksum already exists
    [1, _, $=, $0, $1 | _] ->
      Line ;% A badly formatted checksum already exists
    _ -> 
      CS = integer_to_list(lists:sum(Line) rem 256),
      CSString = add_zeroes(CS),
      Line ++ "10=" ++ CSString ++ [1]
  end.

add_zeroes([C]) -> [$0, $0, C];
add_zeroes([C1, C2]) -> [$0, C1, C2];
add_zeroes([C1, C2, C3]) -> [C1, C2, C3].

