%% TODO:
%%   - Use source_filename's path + search_path + erlang search path for
%%     finding externals (build into zml?)
%%   - Figure out list of zss files
%%   - Pull in zss ASTs
%%   - Cull AST based on ZML AST
%%   - (Future) build list of images from culled zss and do image processing
%%   - Render css inline and attach to AST
 
-module(zs_html_zss_images).

-export([process/5]).


process(ID, Attr, Children, AST, Options) ->
  SearchPaths = get_search_paths(Options),
  io:format("~p", [SearchPaths]),
  AST.

get_search_paths(Options) ->
    case proplists:get_value(source_filename, Options, none) of
      none -> [];
      V -> filename:dirname(V)
    end ++
    case proplists:get_value(path, Options, none) of
      none -> [];
      Vs -> Vs
    end ++
    code:get_path().

