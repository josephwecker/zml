
-module(zml_hand_parser).

-export([parse/2]).


parse(Tokens, Options) ->
  parse(Tokens, [], Options).


parse([], CurrLvl, _Options) -> lists:reverse(CurrLvl);

parse([{end_of_file,_}], CurrLvl, Options) ->
  parse([], CurrLvl, Options);

parse([{dedent,_}|{end_of_file,_}], CurrLvl, Options) ->
  parse([], CurrLvl, Options);

parse([{dedent,_}|T], CurrLvl, Options) ->
  {T, parse([], CurrLvl, Options)};

parse([{start_code, _} | T], CurrLvl, Options) ->
  {T2, InnerCode} = pull_code_children(T),
  {T3, LChildren} = pull_line_children(T2, Options),
  {T4, Children} = pull_children(T3),
  CodeTag = {InnerCode, code, [], LChildren ++ Children},
  parse(T4, [CodeTag | CurrLvl], Options);

parse([{start_tag, _, Type} |
    [{string, _, TagText} | T]], CurrLvl, Options) ->
  {T2, Attr} = pull_attributes(T),
  {T3, LChildren} = pull_line_children(T2, Options),
  {T4, Children} = pull_children(T3),
  Tag = tricky_attributes(TagText, Type, Attr,
                          LChildren ++ Children, Options),
  parse(T4, [Tag | CurrLvl], Options);

parse([{string, _, Text} | T], CurrLvl, Options) ->
  parse(T, [Text | CurrLvl], Options);

parse([{newline, _} | T], CurrLvl, Options) ->
  parse(T, [newline | CurrLvl], Options).


pull_attributes([{start_attrs,_} | T]) ->
  pull_inner_attributes(T, [], [], []);

pull_attributes(AnythingElse) ->  % No attributes for this tag
  {AnythingElse, []}.

pull_inner_attributes([{attr_delim, _} | T], [], [], AttrAcc) ->
  pull_inner_attributes(T, [], [], AttrAcc);

pull_inner_attributes([{attr_delim, _} | T], CAName, [], AttrAcc) -> % weird edge case
  pull_inner_attributes(T, [], [], [{string:to_lower(CAName), []} | AttrAcc]);

pull_inner_attributes([{attr_delim, _} | T], [], [NewName], AttrAcc) -> % weird edge case
  pull_inner_attributes(T, NewName, [], AttrAcc);

pull_inner_attributes([{attr_delim, _} | T], CAName, [NewName | CAVals], AttrAcc) ->
  pull_inner_attributes(T, NewName, [],
    [{string:to_lower(CAName), lists:reverse(CAVals)} | AttrAcc]);

pull_inner_attributes([{string, _, Text} | T], CAName, CAVals, AttrAcc) ->
  pull_inner_attributes(T, CAName, [Text | CAVals], AttrAcc);

pull_inner_attributes([{finish_attrs,_} | T], [], [], AttrAcc) ->
  {T, AttrAcc};
pull_inner_attributes([{finish_attrs,_} | T], CAName, CAVals, AttrAcc) ->
  {T, [{CAName, lists:reverse(CAVals)} | AttrAcc]}.

pull_code_children(Toks) ->
  pull_code_children(Toks, []).
pull_code_children([],Acc) ->
  {[], lists:reverse(Acc)};
pull_code_children([{string, _, Text} | T], Acc) ->
  pull_code_children(T, [Text | Acc]);
pull_code_children([{finish_code, _} | T], Acc) ->
  {T, lists:reverse(Acc)}.

pull_line_children(Toks, Options) ->
  pull_line_children(Toks, [], Options).
pull_line_children([], Acc, _Options) ->
  {[], lists:reverse(Acc)};
pull_line_children([{newline,_}|T], Acc, _Options) ->
  {T, lists:reverse([newline | Acc])};
pull_line_children([{end_of_file,_}|T], Acc, _Options) ->
  {T, lists:reverse(Acc)};
pull_line_children([{inline_delim,_}|T], Acc, _Options) ->
  {T, lists:reverse(Acc)};
pull_line_children([{string, _, Text} | T], Acc, Options) ->
  pull_line_children(T, [Text | Acc], Options);
pull_line_children([{start_tag, _, Type} |
    [{string, _, TagText} | T]], Acc, Options) ->
  {T2, Attr} = pull_attributes(T),
  {T3, LChildren} = pull_line_children(T2, Options),
  Tag = tricky_attributes(TagText, Type, Attr, LChildren, Options),
  pull_line_children(T3, [Tag | Acc], Options).


pull_children([{indent, _}|T]) -> parse(T, []);
pull_children(AnythingElse) -> {AnythingElse, []}.


% Expands divs and adds special id to specials
tricky_attributes(Tag, Type, Attr, Children, Options) ->
  {Name2, Attr2} = tricky_attributes(lists:reverse(Tag), [], Attr, Options),
  case Type of
    class ->
      {"div", normal, zml:append_attr(Attr2, {"class", [Name2]}), Children};
    id ->
      {"div", normal, zml:append_attr(Attr2, {"id", [Name2]}), Children};
    special ->
      ID = integer_to_list(erlang:phash2(make_ref())),
      handle_special({{Name2, ID}, special, Attr2, Children}, Options);
    _ ->
      {Name2, Type, Attr2, Children}
  end.


tricky_attributes([], CurrName, Attr, _Options) -> {CurrName, Attr};

tricky_attributes([$.|T], CurrName, Attr, Options) ->
  tricky_attributes(T, [],
    zml:append_attr(Attr, {"class", [CurrName]}), Options);

tricky_attributes([$#|T], CurrName, Attr, Options) ->
  tricky_attributes(T, [],
    zml:append_attr(Attr, {"id", [CurrName]}), Options);

tricky_attributes([H|T], CurrName, Attr, Options) ->
  tricky_attributes(T, [H | CurrName], Attr, Options).


handle_special({{Name, _}, special, _, _} = Node, Options) ->
  ModuleName = list_to_atom("zml_special_" ++ string:to_lower(Name)),
  {module, Module} = code:ensure_loaded(ModuleName),
  case erlang:function_exported(Module, process_node, 2) of
    true -> Module:process_node(Node, Options);
    _ -> Node
  end.

