%% -*- erlang-indent-level: 2 -*-
%% @author Daniel Luna <daniel@lunas.se>
%% @copyright 2011 Daniel Luna

-module(fix_generate).
-author('Daniel Luna <daniel@lunas.se>').
-export([generate/1]).

generate(XmlFile) ->
  {Xml, ""} = xmerl_scan:file(XmlFile),
  io:format("~p", [Xml]).
