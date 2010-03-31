-module(zml_hand_parser).

-export([parse/2]).

parse(Tokens, Options) ->
  parse(Tokens, [], Options).

parse([{end_of_file,_}], CurrLvl, Options) ->
  parse([], CurrLvl, Options);
parse([], CurrLvl, _Options) ->
  lists:reverse(CurrLvl);
parse([{dedent,_}|{end_of_file,_}], CurrLvl, _Options) ->
  lists:reverse(CurrLvl);
parse([{dedent,_}|T], CurrLvl, _Options) ->
  {T, lists:reverse(CurrLvl)};
parse([{start_code, _} | T], CurrLvl, Options) ->
  {T2, InnerCode} = pull_code_children(T),
  {T3, LChildren} = pull_line_children(T2),
  {T4, Children} = pull_children(T3),
  CodeTag = {InnerCode, code, [], LChildren ++ Children},
  parse(T4, [CodeTag | CurrLvl], Options);
parse([{start_tag, _, Type} |
    [{string, _, TagText} | T]], CurrLvl, Options) ->
  {T2, Attr} = pull_attributes(T),
  {T3, LChildren} = pull_line_children(T2),
  {T4, Children} = pull_children(T3),
  Tag = tricky_attributes(TagText, Type, Attr, LChildren ++ Children),
  parse(T4, [Tag | CurrLvl], Options);
parse([{string, _, Text} | T], CurrLvl, Options) ->
  parse(T, [Text | CurrLvl], Options);
parse([{newline, _} | T], CurrLvl, Options) ->
  parse(T, CurrLvl, Options).


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

pull_line_children(Toks) ->
  pull_line_children(Toks,[]).
pull_line_children([],Acc) ->
  {[], lists:reverse(Acc)};
pull_line_children([{newline,_}|T],Acc) ->
  {T, lists:reverse(Acc)};
pull_line_children([{end_of_file,_}|T],Acc) ->
  {T, lists:reverse(Acc)};
pull_line_children([{inline_delim,_}|T],Acc) ->
  {T, lists:reverse(Acc)};
pull_line_children([{string, _, Text} | T], Acc) ->
  pull_line_children(T, [Text | Acc]);
pull_line_children([{start_tag, _, Type} |
    [{string, _, TagText} | T]], Acc) ->
  {T2, Attr} = pull_attributes(T),
  {T3, LChildren} = pull_line_children(T2),
  Tag = tricky_attributes(TagText, Type, Attr, LChildren),
  pull_line_children(T3, [Tag | Acc]).


pull_children([{indent, _}|T]) ->
  parse(T, []);
pull_children(AnythingElse) ->
  {AnythingElse, []}.

% Expands divs and adds special id to specials
tricky_attributes(Tag, Type, Attr, Children) ->
  {Name2, Attr2} = tricky_attributes(lists:reverse(Tag), [], Attr),
  case Type of
    class ->
      {"div", normal, zml:append_attr(Attr2, {"class", [Name2]}), Children};
    id ->
      {"div", normal, zml:append_attr(Attr2, {"id", [Name2]}), Children};
    special ->
      ID = integer_to_list(erlang:phash2(make_ref())),
      {{Name2, ID}, special, Attr2, Children};
    _ ->
      {Name2, Type, Attr2, Children}
  end.

tricky_attributes([], CurrName, Attr) ->
  {CurrName, Attr};
tricky_attributes([$.|T], CurrName, Attr) ->
  tricky_attributes(T, [], zml:append_attr(Attr, {"class", [CurrName]}));
tricky_attributes([$#|T], CurrName, Attr) ->
  tricky_attributes(T, [], zml:append_attr(Attr, {"id", [CurrName]}));
tricky_attributes([H|T], CurrName, Attr) ->
  tricky_attributes(T, [H | CurrName], Attr).

