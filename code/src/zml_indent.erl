
-module(zml_indent).
-compile(export_all).
-include("zml_tokenizer.hrl").

tokenize_string(Str) ->
  Lines = string:tokens(Str, "\n"), % FIXME: removes empty lines
  {Res, []} = tokenize(Lines, -1, recursive, no_tokenizer, []),
  Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize([], _Indent, _Rec, Tok, Acc) -> {apply_tokenizer(Tok, Acc), []};

tokenize([H|_] = Lines, Indent, Rec, Tok, Acc) ->
  tokenize(get_dent(H), Lines, Indent, Rec, Tok, Acc).


tokenize({_, []}, [_|T], Indent, Rec, Tok, Acc) ->
  tokenize(T, Indent, Rec, Tok, ["" | Acc]);

tokenize({_, [$|, $| | _]}, [_|T], Indent, Rec, Tok, Acc) ->
  tokenize(T, Indent, Rec, Tok, Acc);

tokenize({NewDent, _}, Lines, Indent, _Rec, Tok, Acc)
  when NewDent =< Indent -> {apply_tokenizer(Tok, Acc), Lines};

tokenize({_, Ln}, [_|T], Indent, non_recursive, Tok, Acc) ->
  tokenize(T, Indent, non_recursive, Tok, [Ln | Acc]);

tokenize({NewDent, Ln}, [_|T], Indent, Rec, Tok, Acc) ->
  {NewRec, NewTok, [NewLn | Rest]} = get_tokenizer([Ln | T]),
  case NewTok of
    no_tokenizer -> tokenize(Rest, Indent, Rec, Tok, [NewLn | Acc]);
    _ -> {L,R} = tokenize(Rest, NewDent, NewRec, NewTok, [NewLn]),
         tokenize(R, Indent, Rec, Tok, [L | Acc])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_dent(Ln) -> get_dent(Ln, 0).
get_dent([H|T], Dent) when ?IS_WHITESPACE(H) -> get_dent(T, Dent + 1);
get_dent(Ln, Dent) -> {Dent, Ln}.

is_alnum(Ch) when Ch >= $0 andalso Ch =< $9 -> true;
is_alnum($_) -> true;
is_alnum($-) -> true;
is_alnum(Ch) -> ?IS_ALPHA(Ch).

% TODO: move to zml_util? zml_tag uses this function, too.
parse_id([Ch | T]) when ?IS_ALPHA(Ch) ->
  {L, R} = lists:splitwith(fun is_alnum/1, T),
  {[Ch | L], R};

parse_id(Ln) -> {[], Ln}.

parse_id_spc(Ln) ->
  {Dent, W} = get_dent(Ln),
  case parse_id(W) of
    {[], _} -> {[], Ln};
    {L,  R} -> {lists:sublist(Ln, Dent + length(L)), R}
  end.

parse_class_attrs([H|T] = Ln, Attr) when ?IS_ATTR(H) ->
  case parse_id(T) of
    {[], _   } -> {Attr, Ln};
    {Id, Rest} -> parse_class_attrs(Rest,
      zml_util:append_attr(Attr, id2attr(H, Id)))
  end;

parse_class_attrs(Ln, Attr) -> {Attr, Ln}.


id2attr($#, Id) -> {"id",    [Id]};
id2attr($., Id) -> {"class", [Id]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_tokenizer(no_tokenizer, Acc) ->
  {_Attr, Body, []} = zml_tag:inline_tags(lists:reverse(Acc), [], 0),
  Body;

apply_tokenizer({tag, {special, Tag} = _Spc, Attr}, Acc) ->
  Toks = case zml_util:call_special(Tag, tokenize, [Tag, Attr, Acc]) of
    function_not_found ->
      {Tag, normal, NewAttr, Res} = apply_tokenizer({tag, Tag, Attr}, Acc),
      {{Tag, 0}, special, NewAttr, Res};
    Node -> Node
  end,
  zml_util:call_special(Tag, process_node, [Toks, []], Toks);

apply_tokenizer({tag, Tag, Attr}, Acc) ->
  {NewAttr, Body, []} = zml_tag:inline_tags(lists:reverse(Acc), Attr, 0),
  {Tag, normal, NewAttr, Body}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_tokenizer([[Ch | W] | T] = Lines) when ?IS_TAG(Ch) ->
  case parse_id(W) of
    {[], _} -> {recursive, no_tokenizer, Lines};
    {Id, RestLn} ->
      {IsRec, HasAttrs, HasClassAttrs} = is_recursive(Ch, Id),
      {{NewType, NewTag, ClassAttr} = Tok, Rest} = case HasClassAttrs of
        has_class_attrs ->
          {Type, Tag, FirstAttr} = get_tag(Ch, Id),
          {NewAttr, NewRestLn} = parse_class_attrs(RestLn, FirstAttr),
          {{Type, Tag, NewAttr}, NewRestLn};
        _ -> {get_tag(Ch, Id), RestLn}
      end,
      case HasAttrs of
        has_attrs ->
          case zml_tag:parse_attr([Rest | T], ClassAttr) of
            {Attr, []} -> {IsRec, {NewType, NewTag, Attr}, [""]};
            {Attr, Body} -> {IsRec, {NewType, NewTag, Attr}, Body}
          end;
        _ -> {IsRec, Tok, [Rest | T]}
      end
  end;

get_tokenizer([[$\\ | [Ch | _] = W] | T]) when ?IS_TAG(Ch) ->
  {recursive, no_tokenizer, [W | T]};

get_tokenizer([]) -> {recursive, no_tokenizer, [""]};

get_tokenizer(Lines) -> {recursive, no_tokenizer, Lines}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_tag($:, Id) -> {tag,           Id,  []};
get_tag($*, Id) -> {tag, {special, Id}, []};
get_tag(Ch, Id) when ?IS_ATTR(Ch) -> {tag, "div", [id2attr(Ch, Id)]}.

is_recursive($*, Tag) ->
  zml_util:call_special(Tag, is_recursive, [],
    {recursive, has_attrs, has_class_attrs});

is_recursive(_, _) -> {recursive, has_attrs, has_class_attrs}.

