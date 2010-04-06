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
  % DEBUG io:format("~n~p~n~n", [DeclaredZSS]),
  Processed = lists:map(fun(Styles) ->
        process_styles(Styles, AST, Options)
    end, DeclaredZSS),
  AST.

get_declared_zss(Attr, Options) ->
  % Explicitly declared
  Declared = lists:map(fun({Type, _Tags}) ->
        {Type, find_styles(
          zml:get_attr_vals(Type, Attr) ++
          zml:get_attr_vals(Type ++ "s", Attr))}
    end, ?STYLESHEET_TAGS),
  % Look for a magic one as well
  Declared2 =
    case proplists:get_value(source_filename, Options) of
      undefined -> Declared;
      SFN ->
        case find_magic_file(SFN, Options) of
          none -> Declared;
          MagicFile -> zml:append_attr(Declared, {"style", MagicFile})
        end
    end,
  % And full libraries
  case zml:get_attr_vals(stylelib, Attr) ++ zml:get_attr_vals(stylelibs, Attr) of
    [] -> Declared2;
    Libs ->
      append_lib_styles(Options, Declared2, Libs)
  end.

find_styles([]) -> [];
find_styles(L) ->
  find_styles(L, []).
find_styles([], Acc) ->
  lists:reverse(Acc);
find_styles([S | T], Acc) ->

append_lib_styles(_Opts, Dec, []) ->
  Dec;
append_lib_styles(Opts, Dec, [Lib | T]) ->
  {ok, Dir} = find_lib_dir(Lib, Opts),
  Dec2 = lists:foldl(fun({Type,_Tags}, Acc) ->
      FName = filename:join([Dir, Type ++ ".zss"]),
      case filelib:is_file(FName) of
        true ->
          zml:append_attr(Acc, {Type, FName});
        false ->
          Acc
      end
    end, Dec, ?STYLESHEET_TAGS),
  append_lib_styles(Opts, Dec2, T).

find_lib_dir(Wanted, Opts) ->
  case proplists:get_value(zml_zss_libs, Opts) of
    undefined -> {error, "zml_zss_libs undefined"};
    RawLibs ->
      Libs = string:tokens(RawLibs, ";,"),
      case lists:dropwhile(fun(E) -> dir_missing_wanted(E,Wanted) end, Libs) of
        [] -> {error, "ZSS library not found"};
        [Found | _] -> {ok, filename:join([Found, Wanted])}
      end
  end.

dir_missing_wanted(TryBaseDir, Lookfor) ->
  TryDir = filename:join([TryBaseDir, Lookfor]),
  not filelib:is_dir(TryDir).

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

% Uses optional magical file-extension fill and search paths to try and find an
% actual file.
find_file(Base, Extension, SearchPaths) ->
  FName = case {filename:extension(Base), Extension} of
      {_, []} -> Base;
      {[], [$. | _]} -> Base ++ Extension;
      {[], Extension} -> Base ++ "." ++ Extension;
      {Extension, Extension} -> Base;
      {[$. | Extension], Extension} -> Base;
      {_, [$. | _]} -> Base ++ Extension;
      _ -> Base ++ "." ++ Extension
    end,
  case filename:pathtype(FName) of
    absolute ->
      case filelib:is_file(FName) of
        true -> {ok, FName};
        false -> {error, "Could not find "++FName}
      end;
    relative ->
      case file:path_open(SearchPaths, FName, [read]) of
        {ok, IOD, FullName} ->
          file:close(IOD),
          {ok, FullName};
        _ -> {error, "Could not find "++FName++" in any of the search paths."}
      end
  end.
