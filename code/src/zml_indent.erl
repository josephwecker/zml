
-module(zml_indent).

-compile(export_all).

-define(IS_WHITESPACE(H),
  (H >= $\x{0009} andalso H =< $\x{000D})
    orelse H == $\x{0020} orelse H == $\x{00A0}).

-define(IS_SPECIAL(H), H == $:
                orelse H == $*
                orelse H == $#
                orelse H == $.).


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
       {NewRec, NewTok, RestLn} = get_tokenizer(Ln),
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

is_alnum(Ch) when Ch >= $0 andalso Ch =< $9 -> true;
is_alnum(Ch) when Ch >= $a andalso Ch =< $z -> true;
is_alnum(Ch) when Ch >= $A andalso Ch =< $Z -> true;
is_alnum($_) -> true;
is_alnum($-) -> true;
is_alnum(_ ) -> false.

parse_id(Ln) -> lists:splitwith(fun is_alnum/1, Ln).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_tokenizer(no_tokenizer, Acc) -> lists:reverse(Acc);

apply_tokenizer({tag, Tag, Attr}, Acc) ->
  {tag, Tag, Attr, lists:reverse(Acc)};

apply_tokenizer({special_tag, "comment", _Attr}, _Acc) -> [];

apply_tokenizer({special_tag, Tag, Attr}, Acc) ->
  {special_tag, Tag, Attr, lists:reverse(Acc)}.

get_tokenizer([H|T] = Ln) when ?IS_SPECIAL(H) ->
  case parse_id(T) of
    {[],  _  } -> {recursive, no_tokenizer, Ln};
    {Id, Rest} -> {is_recursive(H, Id), get_tag(H, Id), Rest}
  end;

get_tokenizer(Ln) -> {recursive, no_tokenizer, Ln}.

get_tag($:, Id) -> {tag,         Id,    []};
get_tag($*, Id) -> {special_tag, Id,    []};
get_tag($#, Id) -> {tag,         "div", [{"class", Id}]};
get_tag($., Id) -> {tag,         "div", [{"id",    Id}]}.

is_recursive($*, "comment") -> non_recursive;
is_recursive(_, _) -> recursive.

