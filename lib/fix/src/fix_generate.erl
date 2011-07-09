%% -*- erlang-indent-level: 2 -*-
%% @author Daniel Luna <daniel@lunas.se>
%% @copyright 2011 Daniel Luna

-module(fix_generate).
-author('Daniel Luna <daniel@lunas.se>').
-export([generate/1]).

-include_lib("xmerl/include/xmerl.hrl").

generate(XmlFile) ->
  {Xml, ""} = xmerl_scan:file(XmlFile),
  Version = version(Xml),
  Header =
    ["%% -*- erlang-indent-level: 2 -*-\n"
     "%% @author Daniel Luna <daniel@lunas.se>\n"
     "%% @copyright 2011 Daniel Luna\n\n"
     "-module(", string:to_lower(Version),
     ").\s"
     "-author('Daniel Luna <daniel@lunas.se>').\n"
     "-export([decode/1]).\n",

     "decode(_) -> ok."],

  io:format("~s", [Header]).

version(#xmlElement{attributes = Attrs}) ->
  lists:flatten(
    [attr(type, Attrs),
     attr(major, Attrs),
     attr(minor, Attrs),
     case attr(servicepack, Attrs) of
       "0" -> [];
       SP -> ["SP", SP]
     end]).

attr(Key, L) ->
  {value, Result} = lists:keysearch(Key, 2, L),
  Result#xmlAttribute.value.
