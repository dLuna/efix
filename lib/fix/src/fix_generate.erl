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
  Xmls = [begin {Xml, ""} = xmerl_scan:file(File), Xml end ||
           File <- Files, filename:extension(File) =:= ".xml"],
  Records = lists:foldl(F, [], Xmls),
  Data = ["%% -*- erlang-indent-level: 2 -*-\n"
          "%% @author Daniel Luna <daniel@lunas.se>\n"
          "%% @copyright 2011 Daniel Luna\n\n",
          [["-record(", camel_case_to_underscore(Name), ", {\n",
            "          ", string:join(Fields, ",\n          "),
            "}).\n\n"] || {Name, Fields} <- Records]],
  io:format("~s", [Data]).

accumulate_transport_record(Xml, []) ->
  accumulate_transport_record(Xml, [{"FixTransport", ["message"]}]);
accumulate_transport_record(Xml, [{"FixTransport", Values}]) ->
  Components = components(Xml),
  Header =
    [camel_case_to_underscore(attr(name, E)) ||
      E <- expanded_fields(children_of_child(header, Xml), Components)],
  Trailer =
    [camel_case_to_underscore(attr(name, E)) ||
      E <- expanded_fields(children_of_child(trailer, Xml), Components)],
  [{"FixTransport", lists:umerge(lists:sort(Header ++ Trailer), Values)}].

accumulate_message_records(Xml, Acc) ->
  Components = components(Xml),
  lists:foldl(fun(Msg, A) -> add_record(Msg, A, Components) end,
              Acc,
              children_of_child(messages, Xml)).

add_record(#xmlElement{name = message} = Msg, Acc, Components) ->
  Name = attr(name, Msg),
  Fields = [camel_case_to_underscore(attr(name, E)) ||
             E <- expanded_fields(Msg#xmlElement.content, Components)],
  case lists:keysearch(Name, 1, Acc) of
    {value, {Name, Values}} ->
      NewValues = lists:umerge(lists:sort(Fields), Values),
      lists:keyreplace(Name, 1, Acc, {Name, NewValues});
    false ->
      [{Name, lists:sort(Fields)} | Acc]
  end;
add_record(#xmlText{}, Acc, _Components) -> Acc.

parser(XmlFile) ->
  {Xml, ""} = xmerl_scan:file(XmlFile),
  MI = module_info(),
  {value, {compile, Compile}} = lists:keysearch(compile, 1, MI),
  {value, {source, Source}} = lists:keysearch(source, 1, Compile),
  {ok, Bin} = file:read_file(filename:dirname(Source) ++ "/fix_generate.aux"),
  Data = [set_values(binary_to_list(Bin), Xml),
          generate_header_parser(Xml),
          generate_message_parser(Xml),
          generate_trailer_parser(Xml),
          generate_enum_to_value(Xml),
          generate_verify_record(Xml)],
  io:format("~s", [Data]).

set_values("%%VERSION%%" ++ Rest, Xml) -> [generate_fix_version(Xml), Rest];
set_values([C | Rest], Xml) -> [C | set_values(Rest, Xml)].

generate_fix_version(Xml) ->
  lists:flatten([string:to_lower(attr(type, Xml)),
                 attr(major, Xml),
                 attr(minor, Xml),
                 case attr(servicepack, Xml) of
                   "0" -> [];
                   SP -> ["sp", SP]
                 end]).

generate_header_parser(Xml) ->
  [[single_field_parser("header", E, Xml) ||
     E <- expanded_fields(children_of_child(header, Xml), components(Xml))],
   "header(Rest, Acc) ->\n"
   "  {Rest, Acc}.\n\n"].

generate_message_parser(Xml) ->
  Enums = field_type_data("MsgType", Xml),
  [[generate_message_typed_dispatcher(attr(description, E), attr(enum, E), Xml)
    || #xmlElement{name = value} = E <- Enums#xmlElement.content],
   "message(MsgType, _Data) ->\n"
   "  throw({unknown_message_type, MsgType}).\n\n",
   [generate_message_type_handler(E, Xml) ||
     #xmlElement{name = message} = E <- children_of_child(messages, Xml)]].

generate_message_typed_dispatcher(Description, Enum, Xml) ->
  Atom = quote_atom(string:to_lower(Description)),
  case [camel_case_to_underscore(attr(name, E)) ||
         #xmlElement{name = message} = E <-
           children_of_child(messages, Xml),
         attr(msgtype, E) =:= Enum] of
    [] ->
      %% FIXME: With FIXT there is a transport layer and a message
      %%        layer which are independent.  This clause matches when
      %%        a type resides in the other layer.
      [];
    [MsgType] ->
      ["message(", Atom, ", Data) ->\n"
       "  ", MsgType, "(Data, #", MsgType, "{});\n"]
  end.

generate_message_type_handler(Msg, Xml) ->
  MsgName = camel_case_to_underscore(attr(name, Msg)),
  [[single_field_parser(MsgName, E, Xml) ||
    E <- expanded_fields(Msg#xmlElement.content, components(Xml))],
   MsgName, "(Rest, Acc) ->\n"
   "  {Rest, verify_record(Acc)}.\n\n"].

generate_trailer_parser(Xml) ->
  [[single_field_parser("trailer", E, Xml) ||
     E <- expanded_fields(children_of_child(trailer, Xml), components(Xml))],
   "trailer(\"\", Acc) ->\n"
   "  verify_record(Acc);\n"
   "trailer([C1, $= | _], Acc) ->\n"
   "  throw({unexpected_field, [C1], Acc});\n"
   "trailer([C1, C2, $= | _], Acc) ->\n"
   "  throw({unexpected_field, [C1, C2], Acc});\n"
   "trailer([C1, C2, C3, $= | _], Acc) ->\n"
   "  throw({unexpected_field, [C1, C2, C3], Acc});\n"
   "trailer([C1, C2, C3, C4, $= | _], Acc) ->\n"
   "  throw({unexpected_field, [C1, C2, C3, C4], Acc});\n"
   "trailer(Data, Acc) ->\n"
   "  throw({unexpected_data, Data, Acc}).\n"].

generate_verify_record(Xml) ->
  Components = components(Xml),
  Header =
    [{camel_case_to_underscore(attr(name, E)), attr(required, E)} ||
      E <- expanded_fields(children_of_child(header, Xml), Components)],
  Trailer =
    [{camel_case_to_underscore(attr(name, E)), attr(required, E)} ||
      E <- expanded_fields(children_of_child(trailer, Xml), Components)],
  Messages =
    [{camel_case_to_underscore(attr(name, Msg)),
      [{camel_case_to_underscore(attr(name, E)), attr(required, E)} ||
        E <- expanded_fields(Msg#xmlElement.content, Components)]} ||
      #xmlElement{name = message} = Msg <- children_of_child(messages, Xml)],
  [[["verify_record(#", Record, "{} = Data) ->\n"
     "  case Data of\n",
     [["    #", Record, "{", Field, " = undefined} ->\n"
       %% FIXME: It seems like mandatory fiels are missing all the
       %%        time.  Including at Wikipedia.

       %% "      throw({missing_mandatory_field, ", Record, ",",  Field,
       %% "});\n"] ||
       "      error_logger:format(\"{missing_mandatory_field, ",
       Record, ",",  Field, "}\", []),\n"
       "      Data;\n"] ||
       {Field, Mandatory} <- Fields,
       Mandatory =:= "Y"],
     "    #", Record, "{} -> Data\n"
     "  end;\n"]  ||
     {Record, Fields} <- [{"fix_transport", Header ++ Trailer} | Messages]],
   "verify_record(Data) ->\n"
   "  throw({unknown_record, Data})."].

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
  [[generate_enum_to_value_field(Field, generate_fix_version(Xml)) ||
     #xmlElement{name = field} = Field <- children_of_child(fields, Xml)],
   "enum_to_value(Type, Value) ->\n"
   "  throw({unknown_enum_value, Type, Value}).\n"].

generate_enum_to_value_field(Field, FixVersion) ->
  N = attr(number, Field),
  case [E || #xmlElement{name = value} = E <- Field#xmlElement.content] of
    [] -> [];
    Enums ->
      {Prefix, Postfix} =
        case attr(type, Field) of
          "CHAR" ->
            case FixVersion of
              "fix40" -> {$", $"};
              "fix41" -> {$", $"};
              _ -> {$$, []}
            end;
          "STRING" -> {$", $"};
          %% FIXME: Some of these need completely different handling
          "MULTIPLESTRINGVALUE" -> {$", $"};
          "MULTIPLEVALUESTRING" -> {$", $"};
          "MULTIPLECHARVALUE" -> {$", $"};
          "BOOLEAN" -> {$$, []};
          "NUMINGROUP" -> {[], []};
          "INT" -> {[], []}
        end,
      [["enum_to_value(\"", N, "\", ", Prefix, attr(enum, E), Postfix, ") ->\n"
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

components(Xml) ->
  try children_of_child(components, Xml)
  catch _:_ -> [] %% No components in early FIX
  end.

%% FIXME: Every place that calls this function should be updated to
%% handle components and repeating groups.  That will decrease the 24k
%% line record definitions, and also add support for repeating groups.
expanded_fields(List, Components) ->
  lists:flatten(
    [case FC of
       #xmlElement{name = group} ->
         expanded_fields(FC#xmlElement.content, Components);
       #xmlElement{name = field} -> FC;
       #xmlElement{name = component} ->
         Name = attr(name, FC),
         [Component] = [C || #xmlElement{} = C <- Components,
                             attr(name, C) =:= Name],
         expanded_fields(Component#xmlElement.content, Components)
     end || #xmlElement{} = FC <- List]).

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
quote_atom([C | Rest]) when C < $a orelse C > $z -> [$', C, Rest, $'];
quote_atom(String) -> String.
