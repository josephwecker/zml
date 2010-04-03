#!/usr/bin/env escript
%% -*- erlang -*-

main(FNames) ->
  case FNames of
    [] -> io:format("~s~n", [zml:compile_stream(standard_io)]);
    _  -> [io:format("~s~n", [zml:compile_file(F)]) || F <- FNames]
  end.

