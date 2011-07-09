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
set_values("%%HEADERPARSER%%" ++ Rest, Xml) ->
  [header_parser(Xml), set_values(Rest, Xml)];
set_values([C | Rest], Xml) ->
  [C | set_values(Rest, Xml)].

version_atom(Xml) ->
  lists:flatten(
    [to_lower(attr(type, Xml)),
     attr(major, Xml),
     attr(minor, Xml),
     case attr(servicepack, Xml) of
       "0" -> [];
       SP -> ["sp", SP]
     end]).

attr(Key, #xmlElement{attributes = Attrs}) -> attr(Key, Attrs);
attr(Key, L) ->
  {value, Result} = lists:keysearch(Key, #xmlAttribute.name, L),
  Result#xmlAttribute.value.

name(Key, #xmlElement{content = Content}) -> name(Key, Content);
name(Key, L) ->
  {value, Result} = lists:keysearch(Key, #xmlElement.name, L),
  Result#xmlElement.content.

header_parser(Xml = #xmlElement{}) ->
  [header(Element, Xml) || #xmlElement{name = field} = Element <-
                             name(header, Xml)].

header(E = #xmlElement{}, Xml) ->
  Name = attr(name, E),
  Field = with_attr_value(name, Name, name(fields, Xml)),
  N = attr(number, Field),
  Type = attr(type, Field),
  ["header(\"", N, "=\" ++ String, Acc) ->\n"
   "  {FieldData, Rest} = fix_read_data:", version_atom(Xml), "(",
   to_lower(Type), ", String),\n"
   "  header(Rest, [{", field_name_to_atom(Name), ", FieldData} | Acc]);\n"].
   
field_name_to_atom([C | Rest]) ->
  [C + 32 | remove_camel_case(Rest)].

remove_camel_case([C | Rest]) when C >= $A, C =< $Z ->
  [$_, C + 32 | remove_camel_case(Rest)];
remove_camel_case([C | Rest]) -> 
  [C | remove_camel_case(Rest)];
remove_camel_case([]) -> [].

to_lower([C | Rest]) when C >= $A, C =< $Z ->
  [C + 32 | to_lower(Rest)];
to_lower([C | Rest]) ->
  [C | to_lower(Rest)];
to_lower([]) -> [].

with_attr_value(Attr, Value, Content) ->
  case [Element || #xmlElement{attributes = Attributes} = Element <- Content,
                   attr(Attr, Attributes) =:= Value] of
    [] -> throw({Attr, Value, Content});
    [V] -> V
  end.
      
