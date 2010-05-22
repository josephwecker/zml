
-module(zml_tag).
-compile(export_all).
-include("zml_tokenizer.hrl").

% tokenize_tag(Lines) -> tokenize_tag(Lines, text, [], []).

tokenize_tag(Lines, Attr) ->
  parse_attr(tokenize_tag(Lines, text, [], []), Attr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize_tag([[$|, $| | _] | T], text, AccL, AccR) ->
  tokenize_tag(T, text, [], add_text(text, AccL, AccR));

tokenize_tag([[$|, Ch | Ln] | T], text, AccL, AccR) when ?IS_QUOTE(Ch) ->
  tokenize_tag([Ln | T], {quote, Ch}, [], add_text(text, AccL, AccR));

tokenize_tag([[Q, $| | Ln] | T], {quote, Q}, AccL, AccR) ->
  tokenize_tag([Ln | T], text, [], add_text(quote, AccL, AccR));

tokenize_tag([[$\\, Q, $| | Ln] | T], {quote, Q}, AccL, AccR) ->
  tokenize_tag([Ln | T], {quote, Q}, [$|, Q | AccL], AccR);

tokenize_tag([], {quote, Q}, AccL, AccR) ->
  tokenize_tag([], text, AccL ++ [Q, $|], AccR);

tokenize_tag([[] | T], {quote,_} = State, AccL, AccR) ->
  tokenize_tag(T, State, [32 | AccL], AccR);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize_tag([[$\\, Ch | Ln] | T], text, AccL, AccR) ->
  tokenize_tag([Ln | T], text, [Ch | AccL], AccR);

tokenize_tag([[Ch | Ln] | T], text, AccL, AccR) when ?IS_WHITESPACE(Ch) ->
  tokenize_tag([Ln | T], text, [], add_text(text, AccL, AccR));

tokenize_tag([[] | T], text, AccL, AccR) ->
  tokenize_tag(T, text, [], [newline | add_text(text, AccL, AccR)]);

tokenize_tag([Tag | T], text, AccL, AccR) when is_tuple(Tag) ->
  tokenize_tag(T, text, [], [Tag | add_text(text, AccL, AccR)]);

tokenize_tag([[Ch | Ln] | T], State, AccL, AccR) ->
  tokenize_tag([Ln | T], State, [Ch | AccL], AccR);

tokenize_tag([], text, AccL, AccR) ->
  lists:reverse(add_text(text, AccL, AccR)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_text( quote, AccL, AccR) -> [lists:reverse(AccL) | AccR];
add_text(_State, [],   AccR) -> AccR;
add_text(_State, AccL, AccR) -> [lists:reverse(AccL) | AccR].

close_bracket($() -> $);
close_bracket($[) -> $];
close_bracket(${) -> $}.

add_attr({attr, Id, Val}, Attr) ->
  zml:append_attr(Attr, {Id, [lists:reverse(Val)]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_attr([[Ch | W] | T] = Lines, Attr) when ?IS_BR_OPEN(Ch) ->
  case parse_attr([W | T], id, {Ch, close_bracket(Ch), 0}, Attr) of
    fail -> {Attr, Lines};
    Res  -> Res
  end;

parse_attr([newline | T], Attr) -> parse_attr(T, Attr);
parse_attr([[]      | T], Attr) -> parse_attr(T, Attr);

parse_attr(Lines, Attr) -> {Attr, Lines}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_attr([newline | T], id, Br, Attr) -> parse_attr(T, id, Br, Attr);
parse_attr([[]      | T], id, Br, Attr) -> parse_attr(T, id, Br, Attr);

parse_attr([W | T], id, Br, Attr) ->
  case zml_indent:parse_id(W) of
    {Id, ":"} -> parse_attr(T, {attr, Id, []}, Br, Attr);
    _ -> fail
  end;

parse_attr([[] | T], {attr, Id, Val}, Br, Attr) ->
  parse_attr(T, {attr, Id, [32 | Val]}, Br, Attr);

parse_attr([newline | T], {attr, Id, Val}, Br, Attr) ->
  parse_attr(T, {attr, Id, [32 | Val]}, Br, Attr);

parse_attr([[Open | W] | T], {attr, Id, Val}, {Open, Close, L}, Attr) ->
  parse_attr([W | T], {attr, Id, [Open | Val]}, {Open, Close, L+1}, Attr);

parse_attr([[Close | W] | T], {attr,_,_} = State, {_, Close, 0}, Attr) ->
  {add_attr(State, Attr), [W | T]};

parse_attr([[Close | W] | T], {attr, Id, Val}, {Open, Close, L}, Attr) ->
  parse_attr([W | T], {attr, Id, [Close | Val]}, {Open, Close, L-1}, Attr);

% FIXME: suboptimal! do not call parse_id() for every character!
parse_attr([[Ch | W] = Word | T], {attr, Id, Val} = State, Br, Attr) ->
  case zml_indent:parse_id(Word) of
    {NewId, ":"} ->
      parse_attr(T, {attr, NewId, []}, Br, add_attr(State, Attr));
    _ -> parse_attr([W | T], {attr, Id, [Ch | Val]}, Br, Attr)
  end.

% parse_attr(_Lines, _State, _Br, _Attr) -> fail.

