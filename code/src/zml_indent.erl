
-module(zml_indent).

-compile(export_all).

-define(IS_WHITESPACE(H),
  (H >= $\x{0009} andalso H =< $\x{000D})
    orelse H == $\x{0020} orelse H == $\x{00A0}).


tokenize_string(Str) ->
  Lines = string:tokens(Str, "\n"),
  {Res, []} = tokenize(Lines, 0, recursive, no_tokenizer, []),
  Res.

tokenize([], _Indent, _Rec, Tok, Acc) ->
  {apply_tokenizer(Tok, Acc), []};

tokenize([H | T] = Lines, Indent, Rec, Tok, Acc) ->
  {NewDent, Ln} = get_dent(H),
  if NewDent < Indent ->
       {apply_tokenizer(Tok, Acc), Lines};
     Rec =:= non_recursive ->
       tokenize(T, Indent, Rec, Tok, [Ln | Acc]);
     true ->
       {NewRec, NewTok, RestLn} = get_tokenizer(Rec, Ln),
       case NewTok of
         no_tokenizer ->
           tokenize(T, Indent, Rec, Tok, [RestLn | Acc]);
         _ ->
           {L, R} = tokenize(T, NewDent, NewRec, NewTok, [RestLn]),
           tokenize(R, Indent, Rec, Tok, [L | Acc])
       end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_dent(Ln) -> get_dent(Ln, 0).
get_dent([H|T], Dent) when ?IS_WHITESPACE(H) -> get_dent(T, Dent + 1);
get_dent(Ln, Dent) -> {Dent, Ln}.


apply_tokenizer(no_tokenizer, Acc) -> lists:reverse(Acc).


get_tokenizer(non_recursive, Ln) ->
  {non_recursive, no_tokenizer, Ln};

get_tokenizer(recursive, Ln) ->
  {recursive, no_tokenizer, Ln}.

