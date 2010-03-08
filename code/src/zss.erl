-module(zss).

-export([cstr/1, compile/1, output_css/1]).

cstr(InStr) ->
  zss_parser:parse(zss_tokenizer:tokenize_str(InStr)).

compile(FName) ->
  zss_parser:parse(zss_tokenizer:tokenize_file(FName)).

output_css(Rules) ->
  lists:flatten(lists:reverse(output_css(Rules, []))).
output_css([], Acc) ->
  Acc;
output_css([{Sels, Atts} | T], Acc) ->
  SelLine = string:join(Sels,","),
  AttString = output_atts(Atts),
  output_css(T, [[SelLine, "{", AttString, "}"] | Acc]).

output_atts(Atts) ->
  string:join(lists:reverse(output_atts(Atts, [])),";").
output_atts([], Acc) ->
  Acc;
output_atts([{Key, Val} | T], Acc) ->
  output_atts(T, [[Key,":",Val] | Acc]).
