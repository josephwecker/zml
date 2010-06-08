
-module(zml_tag).
-compile(export_all).
-include("zml_tokenizer.hrl").


tokenize_tag(Lines, Attr, Level) ->
  {NewAttr, R} = parse_attr(Lines, Attr),
  {Body, Rest} = tokenize_tag(R, Level, [], []),
  {NewAttr, Body, Rest}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize_tag([[$|, $| | _] | T], Level, AccL, AccR) ->
  tokenize_tag(T, Level, [], add_text(AccL, AccR));

tokenize_tag([[$|, Q | Ln] | T], Level, AccL, AccR) when ?IS_QUOTE(Q) ->
  {L,R} = parse_quote([Ln | T], Q, AccL),
  tokenize_tag(R, Level, L, AccR);

tokenize_tag([[$\\, Ch | Ln] | T], Level, AccL, AccR)
    when ?IS_TAG(Ch) orelse Ch == $| orelse Ch == $; ->
  tokenize_tag([Ln | T], Level, [Ch | AccL], AccR);

tokenize_tag([[$; | Ln] | T], Level, AccL, AccR) when Level > 0 ->
  {lists:reverse(add_text(AccL, AccR)), [Ln | T]};

tokenize_tag([[Ch | W] | T] = Lines, Level, AccL, AccR) when ?IS_TAG(Ch) ->
  case zml_indent:get_tokenizer(Lines) of
    {_,no_tokenizer,_} -> tokenize_tag([W | T], Level, [Ch | AccL], AccR);
    {_Rec, {Type, Tag, Attr}, Rest} ->
      {NewAttr, Body, NewRest} = tokenize_tag(Rest, Attr, Level + 1),
      tokenize_tag(NewRest, Level, [],
        [case {Type, Tag} of
           {tag, {special, Id}} -> {{Id, 0}, special, NewAttr, Body};
           {tag,           Id } -> { Id,     normal,  NewAttr, Body}
         end | add_text(AccL, AccR)])
  end;

tokenize_tag([[] | T], 0, AccL, AccR) ->
  tokenize_tag(T, 0, [], [newline | add_text(AccL, AccR)]);

tokenize_tag([[] | T], Level, AccL, AccR) ->
  tokenize_tag(T, Level, [], add_text(AccL, AccR));

tokenize_tag([Tag | T], Level, AccL, AccR) when is_tuple(Tag) ->
  tokenize_tag(T, Level, [], [Tag | add_text(AccL, AccR)]);

tokenize_tag([[Ch | Ln] | T], Level, AccL, AccR) ->
  tokenize_tag([Ln | T], Level, [Ch | AccL], AccR);

tokenize_tag([], _Level, AccL, AccR) ->
  {lists:reverse(add_text(AccL, AccR)), []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_attr([[Ch | W] | T], Attr) when ?IS_BR_OPEN(Ch) ->
  parse_attr([W | T], none, [], [], {Ch, close_bracket(Ch), 0}, Attr);

parse_attr([[Ch | W] | T], Attr) when ?IS_WHITESPACE(Ch) ->
  parse_attr([W | T], Attr);

parse_attr([[] | T], Attr) -> parse_attr(T, Attr);

parse_attr(Lines, Attr) -> {Attr, Lines}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_attr([[] | T], Id, AccL, AccR, Br, Attr) ->
  parse_attr(T, Id, [], add_text(AccL, AccR), Br, Attr);

parse_attr([[Ch | W] | T], Id, AccL, AccR, Br, Attr) when ?IS_WHITESPACE(Ch) ->
  parse_attr([W | T], Id, [], add_text(AccL, AccR), Br, Attr);

parse_attr([[$|, $| | _] | T], Id, AccL, AccR, Br, Attr) ->
  parse_attr([[] | T], Id, AccL, AccR, Br, Attr);

parse_attr([[$|, Ch | W] | T], Id, AccL, AccR, Br, Attr) when ?IS_QUOTE(Ch) ->
  {L,R} = parse_quote([W | T], Ch, AccL),
  parse_attr(R, Id, L, AccR, Br, Attr);

parse_attr([[Open | W] | T], Id, AccL, AccR, {Open, Close, L}, Attr) ->
  parse_attr([W | T], Id, [Open | AccL], AccR, {Open, Close, L+1}, Attr);

parse_attr([[Close | W] | T], Id, AccL, AccR, {_, Close, 0}, Attr) ->
  {add_attr(Id, AccL, AccR, Attr), [W | T]};

parse_attr([[Close | W] | T], Id, AccL, AccR, {Open, Close, L}, Attr) ->
  parse_attr([W | T], Id, [Close | AccL], AccR, {Open, Close, L-1}, Attr);

parse_attr([[$\\, Close | W] | T], Id, AccL, AccR, {_,Close,_} = Br, Attr) ->
  parse_attr([W | T], Id, [Close | AccL], AccR, Br, Attr);

parse_attr([[$\\, Ch | W] | T], Id,
    AccL, AccR, Br, Attr) when ?IS_WHITESPACE(Ch) orelse Ch == $| ->
  parse_attr([W | T], Id, [Ch | AccL], AccR, Br, Attr);

parse_attr([W | T], Id, AccL, AccR, Br, Attr) ->
  case zml_indent:parse_id(W) of
    {[_|_] = NewId, [$:, Ch | R]} when ?IS_WHITESPACE(Ch) ->
      parse_attr([R | T], NewId, [], [], Br, add_attr(Id, AccL, AccR, Attr));
    {[_|_] = NewId, ":"} ->
      parse_attr(T, NewId, [], [], Br, add_attr(Id, AccL, AccR, Attr));
    {[_|_] = NewId, R} ->
      parse_attr([R | T], Id, append_rev(NewId, AccL), AccR, Br, Attr);
    {[], [Ch | R]} ->
      parse_attr([R | T], Id, [Ch | AccL], AccR, Br, Attr)
  end;

parse_attr([], Id, AccL, AccR, _Br, Attr) ->
  {add_attr(Id, AccL, AccR, Attr), []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_quote([[Q, $| | W] | T], Q, Acc) -> {Acc, [W | T]};
parse_quote([[Ch | W] | T], Q, Acc) -> parse_quote([W | T], Q, [Ch | Acc]);
parse_quote([[] | T], Q, Acc) -> parse_quote(T, Q, [32 | Acc]);
parse_quote([], _, Acc) -> {Acc, []}.


add_text([],   AccR) -> AccR;
add_text(AccL, AccR) -> [lists:reverse(AccL) | AccR].


add_attr(none, _AccL, _AccR, Attr) -> Attr;

add_attr(Id, AccL, AccR, Attr) ->
 zml:append_attr(Attr, {Id, lists:reverse(add_text(AccL, AccR))}).


append_rev([H|T], Acc) -> append_rev(T, [H|Acc]);
append_rev([],    Acc) -> Acc.


close_bracket($() -> $);
close_bracket($[) -> $];
close_bracket(${) -> $}.

