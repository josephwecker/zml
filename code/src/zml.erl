-module(zml).

% Main function
-export([
    compile_file/1,
    compile_file/2,
    compile_files/0,
    compile_files/1,
    compile_stream/1,
    compile_stream/2,
    compile_string/1,
    compile_string/2
  ]).

% Utilities for special handlers:
-export([
    find_magic_file/2,
    find_file/3,
    tmp_filename/0,
    tmp_filename/1,
    pull_in_file/2,
    new_tag/3,
    new_tag/4,
    get_tag/2,
    append_attr/2,
    prepend_attr/2,
    get_attr_vals/2,
    get_attr_vals/3,
    pop_attr/3,
    replace_tag/3,
    update_tag/5,
    append_children/3,
    get_search_paths/1,
    str/1,
    call_special/3,
    call_special/4
  ]).

-define(OPT_ENV(Desc),
        {proplists:get_value(Desc, Options),
         os:getenv(string:to_upper(atom_to_list(Desc)))}).

compile_files() -> compile_files([]).

compile_files(FLS) ->
  case FLS of
    [] -> io:format("~s~n", [zml:compile_stream(standard_io)]);
    _ -> [io:format("~s~n", [zml:compile_file(F)]) || F <- FLS]
  end.

compile_file(InFile) -> compile_file(InFile, []).

compile_file(InFile, Options) ->
  SourceFName = filename:absname(InFile),
  Options2 =
    case proplists:lookup(source_filename, Options) of
      none -> [{source_filename, SourceFName} | Options];
      _ -> Options
    end,
  {ok, Bin} = file:read_file(InFile),
  compile_string(binary_to_list(Bin), Options2).

compile_stream(Stream) -> compile_stream(Stream, []).

compile_stream(Stream, Options) ->
  Str = io:get_chars(Stream, "", 1024000),
  compile_string(Str, Options).

compile_string(Str) -> compile_string(Str, []).

compile_string(Str, Options) ->
  Options2 = other_options(Options),
  AST = zml_indent:tokenize_string(Str), % TODO: pass options here
  AST2 = run_specialized_handlers(AST, Options2),
  Template = translate_ast_item(AST2, []),
  zml_render:render(Template,
    proplists:get_value(data, Options2, fake)).

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

%-define(DIR_TMP, ".tmp").
%-define(DIR_JS,  "js").
%-define(DIR_CSS, "css").
%-define(DIR_IMG, "img").
%-define(DIR_DYN, "dynamic").

%set_up_staging() ->
%  {ok, CurrDir} = file:get_cwd(),
%  BaseDir = tmp_filename(),
%  DirMain = filename:join([CurrDir, BaseDir]),
%  DirTmp =  filename:join([DirMain, ?DIR_TMP]),
%  DirJS =   filename:join([DirMain, ?DIR_JS]),
%  DirCSS =  filename:join([DirMain, ?DIR_CSS]),
%  DirImg =  filename:join([DirMain, ?DIR_IMG]),
%  DirDyn =  filename:join([DirMain, ?DIR_DYN]),
%  file:make_dir(DirMain),
%  file:make_dir(DirTmp),
%  file:make_dir(DirJS),
%  file:make_dir(DirCSS),
%  file:make_dir(DirImg),
%  file:make_dir(DirDyn),
%  {DirMain, DirTmp, DirJS, DirCSS, DirImg, DirDyn}.


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
translate_ast_item([{Code,code,[],Children} | T], Acc) ->
  ToAppend = {with, translate_ast_code(Code),
              translate_ast_item(Children, [])},
  translate_ast_item(T, [ToAppend | Acc]);
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


translate_attributes([]) -> "";
translate_attributes(Atts) ->
  lists:foldl(fun out_attr/2, [], Atts).

out_attr({Name, Values}, Acc) ->
  [" ", Name, "=\"", intersperse(Values, " "), "\"" | Acc].

intersperse([H|T], Sep)      -> intersperse(T, Sep, [H]).
intersperse([H|T], Sep, Acc) -> intersperse(T, Sep, [H, Sep | Acc]);
intersperse([],   _Sep, Acc) -> lists:reverse(Acc).

% TODO: move into tokenizer/parser?
translate_ast_code([Code]) ->
  ["with" | Vars] = string:tokens(Code, " "),
  Vars.

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

new_tag(Name, Attr, Children) when is_atom(Name) ->
  new_tag(atom_to_list(Name), normal, Attr, Children);
new_tag(Name, Attr, Children) when is_tuple(Name) ->
  new_tag(Name, special, Attr, Children).

new_tag(Name, Type, Attr, Children) when is_atom(Name) ->
  new_tag(atom_to_list(Name), Type, Attr, Children);
new_tag(Name, Type, Attr, Children) ->
  {Name, Type, Attr, Children}.


%% Recursively replaces any tags with existing Name with the new attributes and
%% children specified. Search is the search path as a list of tag names, such as:
%% [{"html", ID}, "body", "table"] to find the table tag within the body tag
%% within the special html tag with the given ID.  No mechanism yet for
%% wildcard IDs, wildcard tagnames, looking at attributes, or "lookahead" at
%% children.
replace_tag(AST, Search, NewTag) ->
  replace_tag(AST, lists:reverse(Search), NewTag, [], []).

replace_tag([], _, _, _, Acc) ->
  lists:reverse(Acc);

replace_tag([{Name,Tp,Att,Children} | T], Search, NewTag, CurrPath, Acc) ->
  EqPath = lists:sublist([Name | CurrPath], length(Search)),
  case EqPath == Search of
    true ->
      % We won't recurse into new children, because that would inevitably lead
      % to infinite recursion if someone tried to take advantage of it.
      replace_tag(T, Search, NewTag, CurrPath, [NewTag | Acc]);
    false ->
      NewChildren = replace_tag(Children,Search,NewTag, [Name | CurrPath],[]),
      replace_tag(T, Search, NewTag, CurrPath,
        [{Name,Tp,Att,NewChildren} | Acc])
  end;

replace_tag([H|T], Search, NewTag, CurrPath, Acc) ->
  replace_tag(T, Search, NewTag, CurrPath, [H | Acc]).


%% Shortcut for basically changing the attributes / children of a specific tag.
update_tag(AST, [F | _] = Search, Type, NewAttr, NewChildren) when
    is_list(Search) and (is_list(F) or is_tuple(F)) ->
  replace_tag(AST, Search, new_tag(lists:last(Search), Type, NewAttr, NewChildren));
update_tag(AST, Name, Type, NewAttr, NewChildren) ->
  replace_tag(AST, [Name], new_tag(Name, Type, NewAttr, NewChildren)).

append_children(AST, Search, NewChildren) ->
  {_,Type,Attr,Children} = get_tag(AST, Search),
  update_tag(AST, Search, Type, Attr, Children ++ NewChildren).

%% Similar to replace_tag, only is stops when it finds it and returns it
%% instead of returning a rebuilt full AST.
get_tag(AST, Search) ->
  get_tag(AST, lists:reverse(Search), []).

get_tag([], _, _) -> undefined;

get_tag([{Name,_,_,Children} = Tag | T], Search, CurrPath) ->
  EqPath = lists:sublist([Name | CurrPath], length(Search)),
  case EqPath == Search of
    true ->
      Tag;
    false ->
      case get_tag(Children, Search, [Name | CurrPath]) of
        undefined ->
          get_tag(T, Search, CurrPath);
        FoundTag ->
          FoundTag
      end
  end;

get_tag([_|T], Search, CurrPath) -> get_tag(T, Search, CurrPath).


append_attr([{K1,V1} | Attributes], {K1,V2}) ->
  [{K1, V1 ++ V2} | Attributes];
append_attr([KV1 | Attributes], KV2) ->
  [KV1 | append_attr(Attributes, KV2)];
append_attr([], KV2) ->
  [KV2].

prepend_attr([{K1,V1} | Attributes], {K1,V2}) ->
  [{K1, V2 ++ V1} | Attributes];
prepend_attr([KV1 | Attributes], KV2) ->
  [KV1 | prepend_attr(Attributes, KV2)];
prepend_attr([], KV2) ->
  [KV2].

get_attr_vals(Find, Attr) ->
  case proplists:get_value(str(Find), Attr, none) of
    none -> [];
    V -> V
  end.
get_attr_vals(Find, Attr, [H|_] = Default) when is_integer(H) ->
  proplists:get_value(str(Find), Attr, [Default]);
get_attr_vals(Find, Attr, Default) when is_list(Default) ->
  proplists:get_value(str(Find), Attr, [str(V) || V <- Default]);
get_attr_vals(Find, Attr, Default) ->
  proplists:get_value(str(Find), Attr, [str(Default)]).

pop_attr(Find, Attr, Default) ->
  Key = str(Find),
  Val = proplists:get_value(Key, Attr, [str(Default)]),
  {proplists:delete(Key, Attr), Val}.

str(A) when is_atom(A) ->
  atom_to_list(A);
str(A) ->
  A.

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

call_special(Tag, Func, Args) ->
  call_special(Tag, Func, Args, function_not_found).

call_special(Tag, Func, Args, Default) ->
  ModuleName = list_to_atom("zml_special_" ++ string:to_lower(Tag)),
  case code:ensure_loaded(ModuleName) of
    {module, Module} ->
      case erlang:function_exported(Module, Func, length(Args)) of
        true -> apply(Module, Func, Args);
        _    -> Default
      end;
    {error, _Err} -> Default
  end.

