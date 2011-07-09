%% -*- erlang-indent-level: 2 -*-
%% @author Daniel Luna <daniel@lunas.se>
%% @copyright 2011 Daniel Luna

-module(fix_generate).
-author('Daniel Luna <daniel@lunas.se>').
-export([generate/1]).

-include_lib("xmerl/include/xmerl.hrl").

generate(XmlFile) ->
  {Xml, ""} = xmerl_scan:file(XmlFile),
  %% FIXME: This is ugly, but works in a directory independent way
  %% most of the time.
  MI = module_info(),
  {value, {compile, Compile}} = lists:keysearch(compile, 1, MI),
  {value, {source, Source}} = lists:keysearch(source, 1, Compile),
  {ok, Bin} = file:read_file(filename:dirname(Source) ++ "/fix_generate.aux"),
  Parser = binary_to_list(Bin),
  Result = set_values(Parser, Xml),
  io:format("~s", [Result]).

set_values([], _) -> [];
set_values("%%VERSIONATOM%%" ++ Rest, Xml) ->
  [version_atom(Xml), set_values(Rest, Xml)];
set_values([C | Rest], Xml) ->
  [C | set_values(Rest, Xml)].

version_atom(#xmlElement{attributes = Attrs}) ->
  lists:flatten(
    ["fix",
     attr(major, Attrs),
     attr(minor, Attrs),
     case attr(servicepack, Attrs) of
       "0" -> [];
       SP -> ["sp", SP]
     end]).

attr(Key, L) ->
  {value, Result} = lists:keysearch(Key, 2, L),
  Result#xmlAttribute.value.
