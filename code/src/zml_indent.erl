
-module(zml_indent).

-compile(export_all).

-define(IS_WHITESPACE(H),
  (H >= $\x{0009} andalso H =< $\x{000D})
    orelse H == $\x{0020} orelse H == $\x{00A0}).


tokenize_string(Str) ->
  Lines = string:tokens(Str, "\n"),
  {Res, []} = tokenize(Lines, 0, recursive, []),
  Res.

tokenize([], _Indent, _Rec, Acc) -> {lists:reverse(Acc), []};

tokenize([H | T] = Lines, Indent, Rec, Acc) ->
  {NewDent, Ln} = get_dent(H),
  if NewDent < Indent ->
       {lists:reverse(Acc), Lines};
     NewDent == Indent orelse Rec =:= non_recursive ->
       tokenize(T, Indent, Rec, [Ln | Acc]);
     NewDent > Indent ->
       {L, R} = tokenize(T, NewDent, Rec, [Ln]),
       tokenize(R, Indent, Rec, [L | Acc])
  end.      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_dent(Ln) -> get_dent(Ln, 0).
get_dent([H|T], Dent) when ?IS_WHITESPACE(H) -> get_dent(T, Dent + 1);
get_dent(Ln, Dent) -> {Dent, Ln}.

