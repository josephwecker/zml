-module(zml_hand_parser).

-export([parse/1]).

parse(Tokens) ->
  parse(Tokens, []).

parse([{end_of_file,_}], CurrLvl) ->
  parse([], CurrLvl);
parse([], CurrLvl) ->
  lists:reverse(CurrLvl);
parse([{dedent,_}|T], CurrLvl) ->
  {T, lists:reverse(CurrLvl)};
parse([{start_tag, _, Type} |
    [{string, _, TagText} | T]], CurrLvl) ->
  {T2, Attr} = pull_attributes(T),
  {T3, LChildren} = pull_line_children(T2),
  {T4, Children} = pull_children(T3),
  Tag = tricky_attributes(TagText, Type, Attr, LChildren ++ Children),
  parse(T4, [Tag | CurrLvl]);

parse([{string, _, Text} | T], CurrLvl) ->
  parse(T, [Text | CurrLvl]);

parse([{newline, _} | T], CurrLvl) ->
  parse(T, CurrLvl).

pull_attributes([{start_attrs,_} | T]) ->
  pull_inner_attributes(T, [], [], []);
pull_attributes(AnythingElse) ->  % No attributes for this tag
  {AnythingElse, dict:new()}.

pull_inner_attributes([{string, _, Text} | T], [], [], AttrAcc) ->
  {CAName2, CAVals2, AttrAcc2} =
    case is_new_attr(Text) of
      {yes, {Name, []}} -> {Name, [], AttrAcc};
      {yes, {Name, Rest}} -> {Name, [Rest], AttrAcc}; 
      nope -> {[], [Text], AttrAcc}
    end,
  pull_inner_attributes(T, CAName2, CAVals2, AttrAcc2);
pull_inner_attributes([{string, _, Text} | T], CAName, CAVals, AttrAcc) ->
  {CAName2, CAVals2, AttrAcc2} =
    case is_new_attr(Text) of
      {yes, {Name, []}} -> {Name, [], [{CAName, lists:reverse(CAVals)} | AttrAcc]};
      {yes, {Name, Rest}} -> {Name, [Rest],[{CAName, lists:reverse(CAVals)} | AttrAcc]}; 
      nope -> {CAName, [Text | CAVals], AttrAcc}
    end,
  pull_inner_attributes(T, CAName2, CAVals2, AttrAcc2);
pull_inner_attributes([{finish_attrs,_} | T], [], [], AttrAcc) ->
  {T, dict:from_list(AttrAcc)};
pull_inner_attributes([{finish_attrs,_} | T], CAName, CAVals, AttrAcc) ->
  {T, dict:from_list([{CAName, lists:reverse(CAVals)} | AttrAcc])}.

is_new_attr(Text) ->
  case string:chr(Text, $:) of
    0 ->
      nope;
    Pos -> 
      {Left, Right} = lists:split(Pos, Text),
      [_ | Backwards] = lists:reverse(Left), % pull off ":"
      {yes, {list_to_atom(lists:reverse(Backwards)), Right}}
  end.

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


tricky_attributes(Tag, Type, Attr, Children) ->
  {Name2, Attr2} = tricky_attributes(lists:reverse(Tag), [], Attr),
  case Type of
    class ->
      {"div", normal, merge_attr(Attr2, [{class, [Name2]}]), Children};
    id ->
      {"div", normal, merge_attr(Attr2, [{id, [Name2]}]), Children};
    _ ->
      {Name2, Type, Attr2, Children}
  end.
 
tricky_attributes([], CurrName, Attr) ->
  {CurrName, Attr};
tricky_attributes([$.|T], CurrName, Attr) ->
  tricky_attributes(T, [], merge_attr(Attr, [{class, [CurrName]}]));
tricky_attributes([$#|T], CurrName, Attr) ->
  tricky_attributes(T, [], merge_attr(Attr, [{id, [CurrName]}]));
tricky_attributes([H|T], CurrName, Attr) ->
  tricky_attributes(T, [H | CurrName], Attr).


merge_attr(Attributes, MoreAttributes) ->
  dict:merge(fun(_K, V1, V2) -> V1 ++ V2 end,
    dict:from_list(MoreAttributes), Attributes). 

