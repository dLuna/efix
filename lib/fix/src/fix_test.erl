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
  {Filename, by_line(file:read_line(Dev), Dev)}.

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
  Line = add_ts(Line1),
  try
    fix:decode(Line),
    ok
  catch
    Type:Reason ->
      {fail, {Type, Reason, pp(Line)}, erlang:get_stacktrace()}
  end.

add_ts("<TIME>" ++ Rest) ->
  timestamp() ++ add_ts(Rest);
add_ts("00000000-" ++ Rest) -> "20110707-" ++ add_ts(Rest);
add_ts([C | Rest]) -> [C | add_ts(Rest)];
add_ts([]) -> [].

timestamp() ->
  {{YYYY, MM, DD}, {Hour, Min, Sec}} = erlang:localtime(),
  lists:flatten(io_lib:format("~4..0w~2..0w~2..0w-~2..0w:~2..0w:~2..0w",
                              [YYYY, MM, DD, Hour, Min, Sec])).

pp([]) -> [];
pp([1 | Rest]) -> "SOH" ++ pp(Rest);
pp([C | Rest]) -> [C | pp(Rest)].
