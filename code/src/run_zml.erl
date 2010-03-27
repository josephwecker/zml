#!/usr/bin/env escript
%% -*- erlang -*-

main(FNames) ->
  [io:format("~s~n", [zml:compile_file(F)]) || F <- FNames].

