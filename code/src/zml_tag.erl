
-module(zml_tag).
-compile(export_all).


tokenize_tag(Lines) -> tokenize_tag(Lines, text, [], []).


tokenize_tag([], text, AccL, AccR) ->
  lists:reverse(add_tag(text, AccL, AccR));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize_tag([[$|, $# | Ln] | T], text, AccL, AccR) ->
  tokenize_tag([Ln | T], comment, [], add_tag(text, AccL, AccR));

tokenize_tag([[$#, $| | Ln] | T], comment, _AccL, AccR) ->
  tokenize_tag([Ln | T], text, [], AccR);

tokenize_tag([], comment, AccL, AccR) ->
  tokenize_tag([], text, AccL ++ "#|", AccR);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize_tag([[$|, $" | Ln] | T], text, AccL, AccR) ->
  tokenize_tag([Ln | T], quote, [], add_tag(text, AccL, AccR));
  
tokenize_tag([[$", $| | Ln] | T], quote, AccL, AccR) ->
  tokenize_tag([Ln | T], text, [], add_tag(quote, AccL, AccR));

tokenize_tag([[$\\, $", $| | Ln] | T], quote, AccL, AccR) ->
  tokenize_tag([Ln | T], quote, [$|, $" | AccL], AccR);

tokenize_tag([], quote, AccL, AccR) ->
  tokenize_tag([], text, AccL ++ "\"|", AccR);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize_tag([[$\\, Ch | Ln] | T], text, AccL, AccR) ->
  tokenize_tag([Ln | T], text, [Ch | AccL], AccR);

tokenize_tag([[Ch | Ln] | T], State, AccL, AccR) ->
  tokenize_tag([Ln | T], State, [Ch | AccL], AccR);

tokenize_tag([[] | T], State, AccL, AccR) ->
  tokenize_tag(T, State, [$\n | AccL], AccR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_tag( quote, AccL, AccR) -> [{quote, lists:reverse(AccL)} | AccR];
add_tag(_State, [],   AccR) -> AccR;
add_tag( State, AccL, AccR) -> [{State, lists:reverse(AccL)} | AccR].

