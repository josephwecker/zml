
-module(zml_indent).

-compile(export_all).

-define(IS_WHITESPACE(H),
  (H >= $\x{0009} andalso H =< $\x{000D})
    orelse H == $\x{0020} orelse H == $\x{00A0}).

-define(IS_ATTR(H),    H == $# orelse H == $.).
-define(IS_SPECIAL(H), H == $: orelse H == $* orelse ?IS_ATTR(H)).

tokenize_string(Str) ->
  Lines = string:tokens(Str, "\n"),
  {Res, []} = tokenize(Lines, -1, recursive, no_tokenizer, []),
  Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize([], _Indent, _Rec, Tok, Acc) -> {apply_tokenizer(Tok, Acc), []};

tokenize([H | T] = Lines, Indent, Rec, Tok, Acc) ->
  {NewDent, Ln} = get_dent(H),
  if NewDent =< Indent -> {apply_tokenizer(Tok, Acc), Lines};
     Rec =:= non_recursive -> tokenize(T, Indent, Rec, Tok, [Ln | Acc]);
     true ->
       {NewRec, NewTok, RestLn} = get_tokenizer(Ln),
       case NewTok of
         no_tokenizer -> tokenize(T, Indent, Rec, Tok, [RestLn | Acc]);
         _ -> {L,R} = tokenize(T, NewDent, NewRec, NewTok, [RestLn]),
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

parse_attrs([], Attr) -> {lists:reverse(Attr), []};

parse_attrs([H|T] = Ln, Attr) when ?IS_ATTR(H) ->
  case parse_id(T) of
    {[], _   } -> {lists:reverse(Attr), Ln};
    {Id, Rest} -> parse_attrs(Rest, [id2attr(H, Id) | Attr])
  end;

parse_attrs(Ln, Attr) -> {lists:reverse(Attr), Ln}.


id2attr($#, Id) -> {"id",    Id};
id2attr($., Id) -> {"class", Id}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_tokenizer(no_tokenizer, Acc) -> lists:reverse(Acc);

apply_tokenizer({tag, Tag, Attr}, Acc) ->
  {tag, Tag, Attr, lists:reverse(Acc)};

apply_tokenizer({special_tag, Tag, Attr}, Acc) ->
  case call_special(Tag, tokenize, [Tag, Attr, Acc]) of
    function_not_found -> {special_tag, Tag, Attr, lists:reverse(Acc)};
    Node -> Node
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_tokenizer([H|T] = Ln) when ?IS_SPECIAL(H) ->
  case parse_id(T) of
    {[], _   } -> {recursive, no_tokenizer, Ln};
    {Id, Rest} ->
      {IsRec, HasAttrs} = is_recursive(H, Id),
      case HasAttrs of
        has_class_attrs ->
          {Type, Tag, Attr} = get_tag(H, Id),
          {NewAttr, RestLn} = parse_attrs(Rest, Attr),
          {IsRec, {Type, Tag, NewAttr}, RestLn};
        _ -> {IsRec, get_tag(H, Id), Rest}
      end
  end;

get_tokenizer(Ln) -> {recursive, no_tokenizer, Ln}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_tag($:, Id) -> {tag,         Id, []};
get_tag($*, Id) -> {special_tag, Id, []};
get_tag(Ch, Id) when ?IS_ATTR(Ch) -> {tag, "div", [id2attr(Ch, Id)]}.

is_recursive($*, Tag) ->
  case call_special(Tag, is_recursive, []) of
    function_not_found -> {recursive, has_class_attrs};
    IsRec -> IsRec
  end;

is_recursive(_, _) -> {recursive, has_class_attrs}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: move to the zml_util module
call_special(Tag, Func, Args) ->
  ModuleName = list_to_atom("zml_special_" ++ string:to_lower(Tag)),
  case code:ensure_loaded(ModuleName) of
    {module, Module} ->
      case erlang:function_exported(Module, Func, length(Args)) of
        true -> apply(Module, Func, Args);
        _    -> function_not_found
      end;
    {error, _Err} -> function_not_found
  end.

