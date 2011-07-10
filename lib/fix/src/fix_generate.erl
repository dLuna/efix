%% -*- erlang-indent-level: 2 -*-
%% @author Daniel Luna <daniel@lunas.se>
%% @copyright 2011 Daniel Luna

-module(fix_generate).
-author('Daniel Luna <daniel@lunas.se>').
-export([parser/1, transport_hrl/1, messages_hrl/1]).

-include_lib("xmerl/include/xmerl.hrl").

transport_hrl(Files) ->
  format_hrl(fun accumulate_transport_record/2, Files).

messages_hrl(Files) ->
  format_hrl(fun accumulate_message_records/2, Files).

format_hrl(F, Files) ->
  Xmls =
    [begin {Xml, ""} = xmerl_scan:file(File), Xml end ||
      File <- Files, filename:extension(File) =:= ".xml"],
  
  Records = lists:foldl(F, [], Xmls),

  Data = 
    [
     "%% -*- erlang-indent-level: 2 -*-\n"
     "%% @author Daniel Luna <daniel@lunas.se>\n"
     "%% @copyright 2011 Daniel Luna\n\n",
     [["-record(", camel_case_to_underscore(Name), ", {\n",
       "          ", string:join(Fields, ",\n          "),
       "}).\n\n"] || {Name, Fields} <- Records]],
  io:format("~s", [Data]).

accumulate_transport_record(_Xml, []) -> [{"FixTransport", []}];
accumulate_transport_record(Xml, [{"FixTransport", Values}]) ->
  Header =
    [camel_case_to_underscore(attr(name, E)) ||
      #xmlElement{name = field} = E <- children_of_child(header, Xml)],
  Trailer =
    [camel_case_to_underscore(attr(name, E)) ||
      #xmlElement{name = field} = E <- children_of_child(trailer, Xml)],
  [{"FixTransport", lists:umerge(lists:sort(Header ++ Trailer), Values)}].

accumulate_message_records(Xml, Acc) ->
  lists:foldl(
    fun add_record/2,
    Acc,
    children_of_child(messages, Xml)).

add_record(#xmlElement{name = message} = Msg, Acc) ->
  Name = attr(name, Msg),
  %% FIXME: Handle component
  Fields = [camel_case_to_underscore(attr(name, E)) ||
             #xmlElement{name = field} = E <- Msg#xmlElement.content],
  %% FIXME: Is this doable without sorting?
  case lists:keysearch(Name, 1, Acc) of
    {value, {Name, Values}} ->
      NewValues = lists:umerge(lists:sort(Fields), Values),
      lists:keyreplace(Name, 1, Acc, {Name, NewValues});
    false ->
      [{Name, lists:sort(Fields)} | Acc]
  end;
add_record(#xmlText{}, Acc) -> Acc.

parser(XmlFile) ->
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
set_values("%%TRAILERPARSER%%" ++ Rest, Xml) ->
  [generate_trailer_parser(Xml), set_values(Rest, Xml)];
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
  Enums = field_type_data("MsgType", Xml),
  [[generate_message_typed_dispatcher(attr(description, E), attr(enum, E), Xml)
    || #xmlElement{name = value} = E <- Enums#xmlElement.content],
   "message(MsgType, _Data) ->\n"
   "  throw({unknown_message_type, MsgType}).\n\n",
   [generate_message_type_handler(E, Xml) ||
     #xmlElement{name = message} = E <- children_of_child(messages, Xml)]].

generate_message_typed_dispatcher(Description, Enum, Xml) ->
  case [camel_case_to_underscore(attr(name, E)) ||
         #xmlElement{name = message} = E <-
           children_of_child(messages, Xml),
         attr(msgtype, E) =:= Enum] of
    [] ->
      %% FIXME: particularly for FIXT where the actual message types
      %% are in the fix50* files.
      Atom = quote_atom(string:to_lower(Description)),
      ["message(", Atom, ", Data) ->\n",
       "  throw({unknown_message_type, ", Atom, "});\n"];
    [MsgType] ->
      ["message(", quote_atom(string:to_lower(Description)), ", Data) ->\n"
       "  ", MsgType, "(Data, []);\n"]
  end.

generate_message_type_handler(Msg, Xml) ->
  MsgName = camel_case_to_underscore(attr(name, Msg)),
  [[single_field_parser(MsgName, Field, Xml) 
    || #xmlElement{name = field} = Field <- Msg#xmlElement.content],
   MsgName, "(Rest, Acc) ->\n"
   "  {Rest, Acc}.\n\n"].

generate_trailer_parser(Xml) ->
  [[single_field_parser("trailer", Element, Xml) ||
     %% FIXME: Handle components
     #xmlElement{name = field} = Element <- children_of_child(trailer, Xml)],
   "trailer(\"\", Acc) ->\n"
   "  Acc.\n"].

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
   "  ", MsgName, "(Rest, Acc#", rec_name(MsgName), "{", FieldKey,
   " = FieldData});\n"].

rec_name("header") -> "fix_transport";
rec_name("trailer") -> "fix_transport";
rec_name(Other) -> Other.

generate_enum_to_value(Xml) ->
  [[generate_enum_to_value_field(Field) ||
     #xmlElement{name = field} = Field <- children_of_child(fields, Xml)],
   "enum_to_value(Type, Value) ->\n"
   "  throw({unknown_enum_value, Type, Value}).\n"].

generate_enum_to_value_field(Field) ->
  N = attr(number, Field),
  case [E || #xmlElement{name = value} = E <- Field#xmlElement.content] of
    [] -> [];
    Enums ->
      Quote = case attr(type, Field) of
                "CHAR" -> $";
                "STRING" -> $";
                %% FIXME: Some of these need completely different
                %% handling
                "MULTIPLESTRINGVALUE" -> $";
                "MULTIPLEVALUESTRING" -> $";
                "MULTIPLECHARVALUE" -> $";
                "BOOLEAN" -> $";
                "NUMINGROUP" -> [];
                "INT" -> []
              end,
      [["enum_to_value(\"", N, "\", ", Quote, attr(enum, E), Quote, ") ->\n"
        "  ", quote_atom(string:to_lower(attr(description, E))), ";\n"] ||
        E <- Enums]
  end.

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
