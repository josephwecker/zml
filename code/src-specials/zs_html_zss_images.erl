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
  Processed = lists:map(fun(Styles) ->
        process_styles(Styles, AST, Options)
    end, DeclaredZSS),
  AST.

get_declared_zss(Attr, Options) ->
  % Explicitly declared
  Declared = lists:map(fun({Type, _Tags}) ->
        {Type,
          zml:get_attr_vals(Type, Attr) ++
          zml:get_attr_vals(Type ++ "s", Attr)}
    end, ?STYLESHEET_TAGS),
  % Look for a magic one as well
  Declared2 =
    case proplists:get_value(source_filename, Options, none) of
      none -> Declared;
      SFN ->
        case find_magic_file(SFN, Options) of
          none -> Declared;
          MagicFile -> zml:append_attr(Declared, {"style", MagicFile})
        end
    end,
  case zml:get_attr_vals(stylelib, Attr) of
    [] -> Declared2;
    Libs ->
      append_lib_styles(Options, Declared2, Libs)
  end.

append_lib_styles(Opts, Dec, []) ->
  Dec;
append_lib_styles(Opts, Dec, [Lib | T]) ->
  Dir = find_lib_dir(Lib, Opts),
  Dec2 = lists:foldl(fun({Type,_Tags}, Acc) ->
      FName = filename:join([Dir, Type ++ ".zss"]),
      case filelib:is_file(FName) of
        true ->
          zml:append_attr(Acc, {Type, FName});
        false ->
          Acc
      end
    end, Dec, ?STYLESHEET_TAGS),
  append_lib_styles(Dec2, T).

find_lib_dir(Wanted, Opts) ->
  % TODO: look in each zml_zss_lib value as directory for Wanted directory-
  % return first one found

% Give back: {Type, InlineCSS}
process_styles({Type, Sheets}, AST, Options) ->
  {Type, []}.

find_magic_file(SourceName, Options) ->
  BaseName = filename:rootname(filename:basename(SourceName)),
  MagicZSSName = BaseName ++ ".zss",
  SearchPaths = zml:get_search_paths(Options),
  case file:path_open(SearchPaths, MagicZSSName, [read]) of
    {ok, IOD, FullName} ->
      file:close(IOD),
      [FullName];
    _ -> none
  end.
