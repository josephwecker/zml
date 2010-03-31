%% TODO:
%%   - Figure out list of zss files
%%   - Pull in zss ASTs
%%   - Cull AST based on ZML AST
%%   - (Future) build list of images from culled zss and do image processing
%%   - Render css inline and attach to AST
 
-module(zs_html_zss_images).

-export([process/5]).


process(_ID, Attr, _Children, AST, Options) ->
  DeclaredZSS = get_declared_zss(Attr, Options),
  io:format("~p", [DeclaredZSS]),
  AST.

get_declared_zss(Attr, Options) ->
  zml:get_attr_vals(style,Attr) ++
  zml:get_attr_vals(styles,Attr) ++
  case proplists:get_value(source_filename, Options, none) of
    none -> [];
    SFN ->
      BaseName = filename:rootname(filename:basename(SFN)),
      MagicZSSName = BaseName ++ ".zss",
      SearchPaths = zml:get_search_paths(Options),
      case file:path_open(SearchPaths, MagicZSSName, [read]) of
        {ok, IOD, FullName} ->
          file:close(IOD),
          [FullName];
        _ -> []
      end
  end.

