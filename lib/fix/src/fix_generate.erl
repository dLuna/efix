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

set_values("%%VERSIONATOM%%" ++ Rest, Xml) ->
  [generate_fix_version(Xml), set_values(Rest, Xml)];
set_values("%%HEADERPARSER%%" ++ Rest, Xml) ->
  [generate_header_parser(Xml), set_values(Rest, Xml)];
set_values("%%MESSAGEPARSER%%" ++ Rest, Xml) ->
  [generate_message_parser(Xml), set_values(Rest, Xml)];
set_values("%%FOOTERPARSER%%" ++ Rest, Xml) ->
  [generate_footer_parser(Xml), set_values(Rest, Xml)];
set_values("%%ENUM_TO_VALUE%%" ++ Rest, Xml) ->
  [generate_enum_to_value(Xml), set_values(Rest, Xml)];
set_values([C | Rest], Xml) ->
  [C | set_values(Rest, Xml)];
set_values([], _) -> [].

generate_fix_version(Xml) ->
  [string:to_lower(attr(type, Xml)),
   attr(major, Xml),
   attr(minor, Xml),
   case attr(servicepack, Xml) of
     "0" -> [];
     SP -> ["sp", SP]
   end].

generate_header_parser(Xml) ->
  [[single_field_parser("header", Element, Xml) ||
     #xmlElement{name = field} = Element <- children_of_child(header, Xml)],
   "header(Rest, Acc) ->\n"
   "  {Rest, Acc}.\n"].

generate_message_parser(Xml) ->
  [[generate_message_typed_dispatcher(attr(name, Element), Xml) ||
     #xmlElement{name = message} = Element <- children_of_child(messages, Xml)],
   "message(MsgType, _Data) ->\n"
   "  throw({unknown_message_type, MsgType}).\n\n",
   [generate_message_type_handler(Element, Xml) ||
     #xmlElement{name = message} = Element <- children_of_child(messages, Xml)]].

generate_message_typed_dispatcher(Name, _Xml) ->
  MsgType = camel_case_to_underscore(Name),
  ["message(", MsgType, ", Data) ->\n"
   "  ", MsgType, "(Data, []);\n"].

generate_message_type_handler(Msg, Xml) ->
  MsgName = camel_case_to_underscore(attr(name, Msg)),
  [[single_field_parser(MsgName, Field, Xml) 
    || #xmlElement{name = field} = Field <- Msg#xmlElement.content],
   MsgName, "(Rest, Acc) ->\n"
   "  {Rest, Acc}.\n"].

generate_footer_parser(_) ->
  %% FIXME
  [].

single_field_parser(MsgName, Field, Xml) ->
  FieldName = attr(name, Field),
  FieldTypeData = field_type_data(FieldName, Xml),
  N = attr(number, FieldTypeData),
  Type = string:to_lower(attr(type, FieldTypeData)),
  FieldKey = camel_case_to_underscore(FieldName),
  FixVersion = generate_fix_version(Xml),
  MaybeEnumToValue =
    case [Enum || #xmlElement{name = value} = Enum <-
                    FieldTypeData#xmlElement.content] of
      [] -> "  FieldData = FieldData0,\n";
      _ -> ["  FieldData = enum_to_value(\"", N, "\", FieldData0),\n"]
    end,
  [MsgName, "(\"", N, "=\" ++ String, Acc) ->\n"
   "  {FieldData0, Rest} = fix_read_data:", FixVersion, "(",
   Type, ", String),\n",
   MaybeEnumToValue,
   "  ", MsgName, "(Rest, [{", FieldKey, ", FieldData} | Acc]);\n"].

generate_enum_to_value(Xml) ->
  [[generate_enum_to_value_field(Field) ||
     #xmlElement{name = field} = Field <- children_of_child(fields, Xml)],
   "enum_to_value(Type, Value) ->\n"
   "  throw({unknown_enum_value, Type, Value}).\n"].

generate_enum_to_value_field(Field) ->
  N = attr(number, Field),
  [["enum_to_value(\"", N, "\", \"", attr(enum, Enum), "\") ->\n"
    "  ", quote_atom(string:to_lower(attr(description, Enum))), ";\n"] ||
    #xmlElement{name = value} = Enum <- Field#xmlElement.content].

%% XML helper functions:

attr(Key, #xmlElement{attributes = Attrs}) ->
  {value, Result} = lists:keysearch(Key, #xmlAttribute.name, Attrs),
  Result#xmlAttribute.value.

children_of_child(Key, #xmlElement{content = Content}) ->
  {value, Result} = lists:keysearch(Key, #xmlElement.name, Content),
  Result#xmlElement.content.

field_type_data(Name, Xml) ->  
  [V] = [Element || #xmlElement{} = Element <- children_of_child(fields, Xml),
                    attr(name, Element) =:= Name],
  V.
      
%% String functions

camel_case_to_underscore([C | Rest]) ->
  [C + 32 | remove_camel_case(Rest)].

remove_camel_case([C | Rest]) when C >= $A, C =< $Z ->
  [$_, C + 32 | remove_camel_case(Rest)];
remove_camel_case([C | Rest]) -> 
  [C | remove_camel_case(Rest)];
remove_camel_case([]) -> [].

quote_atom("and") -> "'and'";
quote_atom("or") -> "'or'";
quote_atom("receive") -> "'receive'";
quote_atom("query") -> "'query'";
quote_atom([C | Rest]) when C < $a orelse C > $z ->
  [$', C, Rest, $'];
quote_atom(String) ->
  String.
