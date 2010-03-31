%% TODO:
%%   - Figure out list of zss files
%%   - Pull in zss ASTs
%%   - Cull AST based on ZML AST
%%   - (Future) build list of images from culled zss and do image processing
%%   - Render css inline and attach to AST
 
-module(zs_html_zss_images).

-export([process/5]).
-include("zml_special_html.hrl").


process(_ID, Attr, _Children, AST, Options) ->
  DeclaredZSS = get_declared_zss(Attr, Options),
  io:format("~p", [DeclaredZSS]),
  AST.

get_declared_zss(Attr, Options) ->
  Declared = lists:map(fun({Type, _Tags}) ->
        {Type,
          zml:get_attr_vals(Type,Attr) ++
          zml:get_attr_vals(Type ++ "s",Attr)}
    end, ?STYLESHEET_TAGS),
  case proplists:get_value(source_filename, Options, none) of
    none ->
      Declared;
    SFN ->
      BaseName = filename:rootname(filename:basename(SFN)),
      MagicZSSName = BaseName ++ ".zss",
      SearchPaths = zml:get_search_paths(Options),
      case file:path_open(SearchPaths, MagicZSSName, [read]) of
        {ok, IOD, FullName} ->
          file:close(IOD),
          zml:append_attr(Declared, {"style", [FullName]});
        _ ->
          Declared
      end
  end.


