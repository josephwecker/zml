-module(zss).

-export([cstr/1]).

cstr(InStr) ->
  zss_parser:parse(zss_tokenizer:tokenize_str(InStr)).

