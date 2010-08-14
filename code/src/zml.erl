-module(zml).

% Main function
-export([
    compile_files/0,
    compile_files/1,
    template_file/1,
    template_file/2,
    template_stream/1,
    template_stream/2,
    template_string/1,
    template_string/2,
    render/1,
    render/2
  ]).

% Utilities for special handlers:
-export([
    find_magic_file/2,
    find_file/3,
    tmp_filename/0,
    tmp_filename/1,
    pull_in_file/2,
    get_search_paths/1
  ]).

-define(OPT_ENV(Desc),
        {proplists:get_value(Desc, Options),
         os:getenv(string:to_upper(atom_to_list(Desc)))}).

compile_files() -> compile_files([]).

compile_files([]) ->
  Template = zml:template_stream(standard_io),
  io:format("~s~n", [zml:render(Template)]);

compile_files(FLS) ->
  lists:foreach(fun(FName) ->
    FNameOut = output_file_name(FName),
    io:format("~s --> ~s~n", [FName, FNameOut]),
    Template = zml:template_file(FName),
    ok = file:write_file(FNameOut, zml:render(Template))
  end, FLS).

% TODO: take output path from the options.
output_file_name(FName) ->
  case string:right(FName, 4) of
    ".zml" -> string:left(FName, string:len(FName) - 4);
    _ -> FName
  end ++ ".html".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

template_file(InFile) -> template_file(InFile, []).

template_file(InFile, Options) ->
  SourceFName = filename:absname(InFile),
  Options2 =
    case proplists:lookup(source_filename, Options) of
      none -> [{source_filename, SourceFName} | Options];
      _ -> Options
    end,
  {ok, Bin} = file:read_file(InFile),
  template_string(binary_to_list(Bin), Options2).

template_stream(Stream) -> template_stream(Stream, []).

template_stream(Stream, Options) ->
  Str = io:get_chars(Stream, "", 1024000),
  template_string(Str, Options).

template_string(Str) -> template_string(Str, []).

template_string(Str, Options) ->
  Options2 = other_options(Options),
  AST = zml_indent:tokenize_string(Str), % TODO: pass options here
  AST2 = run_specialized_handlers(AST, Options2),
  translate_ast_item(AST2, []).


render(Template) -> render(Template, fake).

render(Template, Data) -> zml_render:render(Template, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

other_options(Options) ->
  case ?OPT_ENV(zml_zss_libs) of
    {undefined, V1} when V1 =/= false -> [{zml_zss_libs, V1}];
    _ -> []
  end ++
  case ?OPT_ENV(zml_closure_jar) of
    {undefined, V2} when V2 =/= false -> [{zml_closure_jar, V2}];
    _ -> []
  end ++
  Options.


run_specialized_handlers(AST, Options) ->
  run_specialized_handlers_inner(AST, Options, AST).

run_specialized_handlers_inner([], _, NewAST) -> NewAST;

run_specialized_handlers_inner(
    [{{Name, _ID}, special, _Attr, _Children} = Node | T],
    Options, FullAST) ->
  ModuleName = list_to_atom("zml_special_" ++ string:to_lower(Name)),
  {module, Module} = code:ensure_loaded(ModuleName),
  NewAST = case erlang:function_exported(Module, process_tree, 3) of
    true -> Module:process_tree(Node, FullAST, Options);
    _ -> FullAST
  end,
  run_specialized_handlers_inner(T, Options, NewAST);

run_specialized_handlers_inner([_H|T], Options, FullAST) ->
  run_specialized_handlers_inner(T, Options, FullAST).


translate_ast_item([], Acc) -> lists:reverse(Acc);
translate_ast_item([newline | T], Acc) ->
  translate_ast_item(T, ["\n" | Acc]);
translate_ast_item([{var,_} = Var | T], Acc) ->
  translate_ast_item(T, [Var | Acc]);
% In case a special one still remains, remove ID and pretend it's normal
translate_ast_item([{{Name,_ID},Type,Attributes,Children} | T], Acc) ->
  translate_ast_item([{Name, Type, Attributes, Children} | T], Acc);
translate_ast_item([{with, Attr, Children} | T], Acc) ->
  translate_ast_item(T, [{with, Attr, % Attr truncated in the special handler
    translate_ast_item(Children, [])} | Acc]);
translate_ast_item([{Name,_Type,Attributes,[]} | T], Acc) ->
  ToAppend = ["<", Name, translate_attributes(Attributes), "/>"],
  translate_ast_item(T, [ToAppend | Acc]);
translate_ast_item([{Name,_Type,Attributes,Children} | T], Acc) ->
  ToAppend = [
    "<", Name,
    translate_attributes(Attributes), ">",
    translate_ast_item(Children, []),
    "</", Name, ">"],
  translate_ast_item(T, [ToAppend | Acc]);
translate_ast_item([Str | T], Acc) ->
  translate_ast_item(T, [Str | Acc]).


translate_attributes(Atts) -> lists:foldl(fun out_attr/2, [], Atts).

out_attr({Name, Values}, Acc) ->
  [" ", Name, "=\"", intersperse(Values, " "), "\"" | Acc].

intersperse([H|T], Sep)      -> intersperse(T, Sep, [H]).
intersperse([H|T], Sep, Acc) -> intersperse(T, Sep, [H, Sep | Acc]);
intersperse([],   _Sep, Acc) -> lists:reverse(Acc).

%% -------------------- Utilities for special handlers -----------------------

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

