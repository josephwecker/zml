
-module(zml_tag).
-compile(export_all).
-include("zml_tokenizer.hrl").

tokenize_tag(Lines) -> tokenize_tag(Lines, text, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize_tag([[$|, $| | _] | T], text, AccL, AccR) ->
  tokenize_tag(T, text, [], add_text(text, AccL, AccR));

tokenize_tag([[$|, $# | Ln] | T], text, AccL, AccR) ->
  tokenize_tag([Ln | T], comment, [], add_text(text, AccL, AccR));

tokenize_tag([[$#, $| | Ln] | T], comment, _AccL, AccR) ->
  tokenize_tag([Ln | T], text, [], AccR);

tokenize_tag([], comment, AccL, AccR) ->
  tokenize_tag([], text, AccL ++ "#|", AccR);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize_tag([[$|, Ch | Ln] | T], text, AccL, AccR) when ?IS_QUOTE(Ch) ->
  tokenize_tag([Ln | T], {quote, Ch}, [], add_text(text, AccL, AccR));

tokenize_tag([[Ch, $| | Ln] | T], {quote, Q}, AccL, AccR) when Ch == Q ->
  tokenize_tag([Ln | T], text, [], add_text(quote, AccL, AccR));

tokenize_tag([[$\\, Ch, $| | Ln] | T], {quote, Q}, AccL, AccR) when Ch == Q ->
  tokenize_tag([Ln | T], {quote, Q}, [$|, Ch | AccL], AccR);

tokenize_tag([], {quote, Q}, AccL, AccR) ->
  tokenize_tag([], text, AccL ++ [Q, $|], AccR);

tokenize_tag([[] | T], {quote,_} = State, AccL, AccR) ->
  tokenize_tag(T, State, [$\n | AccL], AccR);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize_tag([[$\\, Ch | Ln] | T], text, AccL, AccR) ->
  tokenize_tag([Ln | T], text, [Ch | AccL], AccR);

tokenize_tag([[Ch | Ln] | T], text, AccL, AccR) when ?IS_WHITESPACE(Ch) ->
  tokenize_tag([Ln | T], text, [], add_text(text, AccL, AccR));

tokenize_tag([[] | T], text, AccL, AccR) ->
  tokenize_tag(T, text, [], [newline | add_text(text, AccL, AccR)]);

tokenize_tag([[Ch | Ln] | T], State, AccL, AccR) ->
  tokenize_tag([Ln | T], State, [Ch | AccL], AccR);

tokenize_tag([], text, AccL, AccR) ->
  lists:reverse(add_text(text, AccL, AccR)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_text( quote, AccL, AccR) -> [lists:reverse(AccL) | AccR];
add_text(_State, [],   AccR) -> AccR;
add_text(_State, AccL, AccR) -> [lists:reverse(AccL) | AccR].

