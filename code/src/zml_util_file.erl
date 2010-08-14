
-module(zml_util_file).

% Utilities for special handlers:
-export([
    find_magic_file/2,
    find_file/3,
    tmp_filename/0,
    tmp_filename/1,
    pull_in_file/2,
    get_search_paths/1
  ]).

tmp_filename() ->
  tmp_filename(".tmp_").
tmp_filename(Pref) ->
  Pref ++ integer_to_list(erlang:phash2(make_ref())).

% Tries to copy a file to the destination.  Tries loading it with curl if a
% normal file copy doesn't seem to work.  Returns ok or {error, Reason}
pull_in_file(Name, DestDirAndName) ->
  TryCurl =
    case string:str(Name, "://") of
      0 ->
        case file:copy(Name, DestDirAndName) of
          {ok, _} -> false;
          {error, _} -> "file://" ++ Name
        end;
      _ -> Name
    end,

  case TryCurl of
    false -> ok;
    _ ->
      Res = string:strip(os:cmd("curl -sS -o '" ++ DestDirAndName ++ "' '"
          ++ TryCurl ++ "'")),
      case Res of
        [] -> ok;
        Error -> {error, Error}
      end
  end.

get_search_paths(Options) ->
  % TODO (optionally if needed in the future)
  % - application parameter
  % - environment variable
  % - command line parameter
  % - config file(s)...
  ["."] ++
  case proplists:get_value(source_filename, Options, none) of
    none -> [];
    V ->
      Main = filename:dirname(V),
      Secondaries = [js, zss, images, scripts, styles, javascript,
        'sample-data', 'test-data'],
      [Main | lists:foldl(fun(S, Acc) ->
            S2 = [Main, "/", S],
            case filelib:is_file(S2) of
              true -> [filename:flatten(S2) | Acc];
              false -> Acc
            end
        end, [], Secondaries)]
  end ++
  case proplists:get_value(path, Options, none) of
    none -> [];
    Vs -> Vs
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

find_magic_file(FindExt, Options) ->
  case proplists:get_value(source_filename, Options) of
    undefined -> none;
    SFN ->
      BaseName = filename:rootname(filename:basename(SFN)),
      case find_file(BaseName, FindExt, get_search_paths(Options)) of
        {ok, FullName} -> FullName;
        _ -> none
      end
  end.

