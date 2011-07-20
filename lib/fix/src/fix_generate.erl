%% -*- erlang-indent-level: 2 -*-
%% @author Daniel Luna <daniel@lunas.se>
%% @copyright 2011 Daniel Luna

-module(fix_generate).
-author('Daniel Luna <daniel@lunas.se>').
-export([parser/1, hrl/1]).

-include_lib("xmerl/include/xmerl.hrl").

hrl(Files) ->
  Xmls = [begin {Xml, ""} = xmerl_scan:file(File), Xml end ||
           File <- Files, filename:extension(File) =:= ".xml"],
  Records = lists:foldl(fun accumulate_records/2, [], Xmls),
  Data = ["%% -*- erlang-indent-level: 2 -*-\n"
          "%% @author Daniel Luna <daniel@lunas.se>\n"
          "%% @copyright 2011 Daniel Luna\n\n",
          [["-record(", Name, ", {\n",
            "          ", string:join(Fields, ",\n          "),
            "}).\n\n"] || {Name, Fields} <- Records]],
  io:format("~s", [Data]).

accumulate_records(Xml, Acc0) ->
  Header = record_fields(header(Xml)),
  Trailer = record_fields(trailer(Xml)),
  Acc1 = update_record_def({"FixTransport", ["message" | Header ++ Trailer]},
                           Acc0),
  Acc2 = update_record_defs(messages(Xml), Acc1),
  Acc3 = update_record_defs(components(Xml), Acc2),
  update_record_defs(repeating_groups(Xml), Acc3).

update_record_defs(Defs, Acc) ->
  lists:foldl(fun update_record_def/2, Acc, Defs).

update_record_def(#xmlText{}, Acc) -> Acc;
update_record_def({undefined, #xmlElement{} = E}, Acc) ->
  update_record_def({attr(name, E), record_fields(E#xmlElement.content)}, Acc);
update_record_def({Prefix, #xmlElement{} = E}, Acc) ->
  update_record_def({Prefix ++ attr(name, E),
                     record_fields(E#xmlElement.content)}, Acc);
update_record_def(#xmlElement{} = E, Acc) ->
  update_record_def({attr(name, E), record_fields(E#xmlElement.content)}, Acc);
update_record_def({Name, Fields}, Acc) ->
  Sorted = lists:sort(Fields),
  orddict:update(camel_case_to_underscore(Name),
                 fun(OldFields) -> lists:umerge(OldFields, Sorted) end,
                 Sorted, Acc).

record_fields(List) ->
  [camel_case_to_underscore(attr(name, E)) || #xmlElement{} = E <- List].

record_fields_required(List) ->
  [{camel_case_to_underscore(attr(name, E)), attr(required, E)} ||
    #xmlElement{} = E <- List].

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
          generate_group_parser(Xml),
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
  [[single_field_parser("header", E, Xml) || E <- header(Xml)],
   "header(Rest, Acc) ->\n"
   "  {Rest, Acc}.\n\n"].

generate_message_parser(Xml) ->
  Enums = field_type_data("MsgType", Xml),
  [[generate_message_typed_dispatcher(attr(description, E), attr(enum, E), Xml)
    || #xmlElement{name = value} = E <- Enums#xmlElement.content],
   "message(MsgType, _Data) ->\n"
   "  throw({unknown_message_type, MsgType}).\n\n",
   [generate_message_type_handler(E, Xml) ||
     #xmlElement{name = message} = E <- messages(Xml)]].

generate_message_typed_dispatcher(Description, Enum, Xml) ->
  Atom = quote_atom(string:to_lower(Description)),
  case [camel_case_to_underscore(attr(name, E)) ||
         #xmlElement{name = message} = E <- messages(Xml),
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
  [[single_field_parser(MsgName, E, Xml) || E <- Msg#xmlElement.content],
   MsgName, "(Rest, Acc) ->\n"
   "  {Rest, verify_record(Acc)}.\n\n"].

generate_trailer_parser(Xml) ->
  [[single_field_parser("trailer", E, Xml) || E <- trailer(Xml)],
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
   "  throw({unexpected_data, Data, Acc}).\n\n"].

generate_group_parser(Xml) ->
  Groups = repeating_groups(Xml),
  FixVersion = generate_fix_version(Xml),
  [begin
     Name = attr(name, Group),
     FuncName = camel_case_to_underscore(
                  case Prefix of undefined -> [];
                    _ -> Prefix end ++ Name),
     [[
       begin
         FieldName = attr(name, Field),
         FieldTypeData = field_type_data(FieldName, Xml),
         N = attr(number, FieldTypeData),
         Type = string:to_lower(attr(type, FieldTypeData)),
         RecElem =  camel_case_to_underscore(FieldName),
         [FuncName, "(\"", N, "=\" ++ String, [Acc | AccRest]) ->\n"
          "  {FieldData, Rest} = fix_read_data:", FixVersion, "(",
          Type, ", String),\n",
          "  case Acc of\n"
          "    #", FuncName, "{", RecElem, " = undefined} ->\n"
          "      ", FuncName, "(Rest, [Acc#", FuncName, "{", RecElem,
          " = FieldData} | AccRest]);\n"
          "    _ ->\n"
          "      ", FuncName, "(Rest, [#", FuncName, "{", RecElem,
          " = FieldData}, Acc | AccRest])\n"
          "  end;\n"
         ]
       end || #xmlElement{name = field} = Field <- Group#xmlElement.content],
      FuncName, "(Rest, Acc) ->\n"
      "  {Rest, Acc}.\n\n"
     ]
   end
   ||
    {Prefix, Group} <- Groups].

generate_verify_record(Xml) ->
  Header = record_fields_required(header(Xml)),
  Trailer = record_fields_required(trailer(Xml)),
  Messages =
    [{camel_case_to_underscore(attr(name, Msg)),
      record_fields_required(Msg#xmlElement.content)} ||
      #xmlElement{name = message} = Msg <- messages(Xml)],
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
   "  throw({unknown_record, Data}).\n\n"].

single_field_parser(_FuncName, #xmlText{}, _Xml) -> [];
single_field_parser(FuncName, #xmlElement{name = component}, Xml) ->
  %% FIXME: do_something;
  [];
single_field_parser(FuncName, Element, Xml) ->
  ElementName = attr(name, Element),
  ElementTypeData = field_type_data(ElementName, Xml),
  N = attr(number, ElementTypeData),
  Type = string:to_lower(attr(type, ElementTypeData)),
  ElementKey = camel_case_to_underscore(ElementName),
  FixVersion = generate_fix_version(Xml),
  case Element of
    #xmlElement{name = field} ->
      MaybeEnumToValue =
        case [Enum || #xmlElement{name = value} = Enum <-
                        ElementTypeData#xmlElement.content] of
          [] -> "  FieldData = FieldData0,\n";
          _ -> ["  FieldData = enum_to_value(\"", N, "\", FieldData0),\n"]
        end,
      [FuncName, "(\"", N, "=\" ++ String, Acc) ->\n"
       "  {FieldData0, Rest} = fix_read_data:", FixVersion, "(",
       Type, ", String),\n",
       MaybeEnumToValue,
       "  ", FuncName, "(Rest, Acc#", rec_name(FuncName), "{", ElementKey,
       " = FieldData});\n"];
    #xmlElement{name = group} ->
      GroupName = [case FuncName of
                     "header" -> "";
                     "trailer" -> "";
                     _ -> FuncName ++ "_"
                   end, camel_case_to_underscore(ElementName)],
      [FuncName, "(\"", N, "=\" ++ String, Acc) ->\n"
       "  {N, Rest0} = fix_read_data:", FixVersion, "(",
       Type, ", String),\n",
       "  {Data, Rest} = ", GroupName, "(Rest0, #", GroupName, "{}),\n"
       %% FIXME: check length?
       "  ", FuncName, "(Rest, Acc#", rec_name(FuncName), "{", ElementKey,
       " = Data});\n"]
  end.

rec_name("header") -> "fix_transport";
rec_name("trailer") -> "fix_transport";
rec_name(Other) -> Other.

generate_enum_to_value(Xml) ->
  [[generate_enum_to_value_field(Field, generate_fix_version(Xml)) ||
     #xmlElement{name = field} = Field <- fields(Xml)],
   "enum_to_value(Type, Value) ->\n"
   "  throw({unknown_enum_value, Type, Value}).\n\n"].

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

header(Xml) -> children_of_child(header, Xml).
messages(Xml) -> children_of_child(messages, Xml).
trailer(Xml) -> children_of_child(trailer, Xml).
components(Xml) ->
  try children_of_child(components, Xml)
  catch _:_ -> [] %% No components in early FIX
  end.
fields(Xml) -> children_of_child(fields, Xml).

repeating_groups(Xml) ->
  lists:flatten(
    [{undefined, E} || #xmlElement{name = group} = E <- header(Xml)] ++
      [[{attr(name, M), E} || #xmlElement{name = group} =
                                E <- M#xmlElement.content]
       || #xmlElement{name = message} = M <- messages(Xml)] ++
      [{undefined, E} || #xmlElement{name = group} = E <- trailer(Xml)] ++
      [[{attr(name, C), E} || #xmlElement{name = group} =
                                E <- C#xmlElement.content]
       || #xmlElement{name = component} = C <- components(Xml)]).

field_type_data(Name, Xml) ->
  {Name, [V]} = {Name, [Element || #xmlElement{} = Element <- fields(Xml),
                                   attr(name, Element) =:= Name]},
  V.

%% String functions

camel_case_to_underscore(String) ->
  tl(remove_camel_case(String)).

remove_camel_case([C | _] = String) when C >= $A, C =< $Z ->
  case String of
    [$C, $P     | Rest] -> "_cp"  ++ remove_camel_case(Rest);
    [$D, $K     | Rest] -> "_dk"  ++ remove_camel_case(Rest);
    [$F, $X     | Rest] -> "_fx"  ++ remove_camel_case(Rest);
    [$G, $T     | Rest] -> "_gt"  ++ remove_camel_case(Rest);
    [$I, $D     | Rest] -> "_id"  ++ remove_camel_case(Rest);
    [$M, $D     | Rest] -> "_md"  ++ remove_camel_case(Rest);
    [$M, $V     | Rest] -> "_mv"  ++ remove_camel_case(Rest);
    [$N, $T     | Rest] -> "_nt"  ++ remove_camel_case(Rest);
    [$T, $S     | Rest] -> "_ts"  ++ remove_camel_case(Rest);
    [$T, $Z     | Rest] -> "_tz"  ++ remove_camel_case(Rest);
    [$A, $C, $K | Rest] -> "_ack" ++ remove_camel_case(Rest);
    [$C, $F, $I | Rest] -> "_cfi" ++ remove_camel_case(Rest);
    [$E, $F, $P | Rest] -> "_efp" ++ remove_camel_case(Rest);
    [$I, $O, $I | Rest] -> "_ioi" ++ remove_camel_case(Rest);
    [$R, $F, $Q | Rest] -> "_rfq" ++ remove_camel_case(Rest);
    [$U, $R, $L | Rest] -> "_url" ++ remove_camel_case(Rest);
    [$X, $M, $L | Rest] -> "_xml" ++ remove_camel_case(Rest);
    [C | Rest] -> "_" ++ [C + 32] ++ remove_camel_case(Rest)
  end;
remove_camel_case([C | Rest]) -> [C | remove_camel_case(Rest)];
remove_camel_case([]) -> [].

quote_atom("and") -> "'and'";
quote_atom("or") -> "'or'";
quote_atom("receive") -> "'receive'";
quote_atom("query") -> "'query'";
quote_atom([C | Rest]) when C < $a orelse C > $z -> [$', C, Rest, $'];
quote_atom(String) -> String.
