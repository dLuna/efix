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
set_values("%%MESSAGEPARSER%%" ++ Rest, Xml) ->
  [message_parser(Xml), set_values(Rest, Xml)];
set_values("%%FOOTERPARSER%%" ++ Rest, Xml) ->
  [footer_parser(Xml), set_values(Rest, Xml)];
set_values("%%ENUM_TO_VALUE%%" ++ Rest, Xml) ->
  [enum_to_value(Xml), set_values(Rest, Xml)];
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
  FieldTypeData = field_type_data(Name, Xml),
  N = attr(number, FieldTypeData),
  Type = attr(type, FieldTypeData),
  AtomName = field_name_to_atom(Name),
  Enum = [Enu || #xmlElement{name = value} = Enu <-
                   FieldTypeData#xmlElement.content],
  ["header(\"", N, "=\" ++ String, Acc) ->\n"
   "  {FieldData0, Rest} = fix_read_data:", version_atom(Xml), "(",
   to_lower(Type), ", String),\n",
   case Enum of
     [] -> "  FieldData = FieldData0,\n";
     _ ->
       ["  FieldData = enum_to_value(", N, ", FieldData0),\n"]
   end,
   "  header(Rest, [{", AtomName, ", FieldData} | Acc]);\n"].
   
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

field_type_data(Name, Xml) ->  
  with_attr_value(name, Name, name(fields, Xml)).

with_attr_value(Attr, Value, Content) ->
  case [Element || #xmlElement{attributes = Attributes} = Element <- Content,
                   attr(Attr, Attributes) =:= Value] of
    [] -> throw({Attr, Value, Content});
    [V] -> V
  end.
      
message_parser(Xml) ->
  [[message_dispatch(attr(name, Element), Xml) ||
     #xmlElement{name = message} = Element <- name(messages, Xml)],
   "message(MsgType, _Data) ->\n"
   "  throw({unknown_message_type, MsgType}).\n\n",
   [message_handler(Element, Xml) ||
     #xmlElement{name = message} = Element <- name(messages, Xml)]].

message_dispatch(Name, _Xml) ->
  Atom = field_name_to_atom(Name),
  ["message(", Atom, ", Data) ->\n"
   "  ", Atom, "(Data, []);\n"].

message_handler(Msg, Xml) ->
  MsgName = field_name_to_atom(attr(name, Msg)),
  [[single_message(MsgName, Field, Xml) 
    || #xmlElement{name = field} = Field <- Msg#xmlElement.content],
   MsgName, "(Rest, Acc) ->\n"
   "  {Rest, Acc}.\n"].


single_message(MsgName, Field, Xml) ->
  FieldName = attr(name, Field),
  FieldTypeData = field_type_data(FieldName, Xml),
  N = attr(number, FieldTypeData),
  Type = attr(type, FieldTypeData),
  AtomName = field_name_to_atom(FieldName),
  Enum = [Enu || #xmlElement{name = value} = Enu <-
                   FieldTypeData#xmlElement.content],
  [MsgName, "(\"", N, "=\" ++ String, Acc) ->\n"
   "  {FieldData0, Rest} = fix_read_data:", version_atom(Xml), "(",
   to_lower(Type), ", String),\n",
   case Enum of
     [] -> "  FieldData = FieldData0,\n";
     _ ->
       ["  FieldData = enum_to_value(\"", N, "\", FieldData0),\n"]
   end,
   "  ", MsgName, "(Rest, [{", AtomName, ", FieldData} | Acc]);\n"].


footer_parser(_) ->
  [].

enum_to_value(Xml) ->
  [[enum(attr(number, Field), Field#xmlElement.content) ||
     #xmlElement{name = field} = Field <- name(fields, Xml)],
   "enum_to_value(Type, Value) ->\n"
   "  throw({unknown_enum_value, Type, Value}).\n"].

enum(N, L) ->
  [enum_str(N, attr(enum, Enum),
            quote_atom(to_lower(attr(description, Enum)))) ||
    #xmlElement{name = value} = Enum <- L].

enum_str(N, Enum, Desc) ->
  ["enum_to_value(\"", N, "\", \"", Enum, "\") ->\n"
   "  ", Desc, ";\n"].

quote_atom("and") -> "'and'";
quote_atom("or") -> "'or'";
quote_atom("receive") -> "'receive'";
quote_atom("query") -> "'query'";
quote_atom([C | Rest]) when C < $a orelse C > $z ->
  [$', C, Rest, $'];
quote_atom(String) ->
  String.
