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
    search_for_file/2,
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
    str/1
  ]).

-define(OPT_ENV(Desc),
  {proplists:get_value(Desc, Options), os:getenv(string:to_upper(atom_to_list(Desc)))}).

compile_files() ->
  compile_files([]).
compile_files(FLS) ->
  case FLS of
    [] -> io:format("~s~n", [zml:compile_stream(standard_io)]);
    _ -> [io:format("~s~n", [zml:compile_file(F)]) || F <- FLS]
  end.

compile_file(InFile) ->
  compile_file(InFile, []).
compile_file(InFile, Options) ->
  SourceFName = filename:absname(InFile),
  Options2 =
    case proplists:lookup(source_filename, Options) of
      none -> [{source_filename, SourceFName} | Options];
      _ -> Options
    end,
  do_compile(fun zml_tokenizer:tokenize_file/1, InFile, Options2).

compile_stream(Stream) ->
  compile_stream(Stream, []).
compile_stream(Stream, Options) ->
  do_compile(fun zml_tokenizer:tokenize_stream/1, Stream, Options).

compile_string(Str) ->
  compile_string(Str, []).
compile_string(Str, Options) ->
  do_compile(fun zml_tokenizer:tokenize_string/1, Str, Options).

do_compile(Tokenizer, Input, Options) ->
  Tokens = Tokenizer(Input),
  Options2 = other_options(Options),
  AST = zml_hand_parser:parse(Tokens, Options2),
  AST2 = run_specialized_handlers(AST, Options2),
  translate_ast_item(AST2, []).

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
run_specialized_handlers_inner([], _, NewAST) ->
  NewAST;
run_specialized_handlers_inner([{{Name,ID}, special, Attr, Children} | T],
    Options, FullAST) ->
  HandlerModule = list_to_atom("zml_special_" ++ string:to_lower(Name)),
  case code:ensure_loaded(HandlerModule) of
    {module, _} ->
      NewAST = HandlerModule:run_handler(ID, Attr, Children, FullAST, Options),
      run_specialized_handlers_inner(T, Options, NewAST);
    _ ->
      erlang:error(["No code found to handle special tag type:",Name])
  end;
run_specialized_handlers_inner([_H|T], Options, FullAST) ->
  run_specialized_handlers_inner(T, Options, FullAST).


translate_ast_item([], Acc) ->
  lists:reverse(Acc);
translate_ast_item([[$< | _] = String | [Next | _] = T], Acc)
when is_list(Next) ->
  translate_ast_item(T, [String | Acc]);
translate_ast_item([String | [Next | _] = T], Acc)
when is_list(String) and is_list(Next) ->
  translate_ast_item(T, [" " | [String | Acc]]);
translate_ast_item([String | T], Acc) when is_list(String) ->
  translate_ast_item(T, [String | Acc]);
% In case a special one still remains, remove ID and pretend it's normal
translate_ast_item([{{Name,_ID},Type,Attributes,Children} | T], Acc) ->
  translate_ast_item([{Name, Type, Attributes, Children} | T], Acc);
translate_ast_item([{Code,code,[],Children} | T], Acc) ->
  ToAppend = ["!!CODE!!",
    string:join(Code, " "),
    "!!",
    translate_ast_item(Children, []),
    "!!END!!"],
  translate_ast_item(T, [ToAppend | Acc]);
translate_ast_item([{Name,_Type,Attributes,[]} | T], Acc) ->
  ToAppend = ["<", Name,
    translate_attributes(Attributes), "/>"],
  translate_ast_item(T, [ToAppend | Acc]);
translate_ast_item([{Name,_Type,Attributes,Children} | T], Acc) ->
  ToAppend = [
    "<", Name,
    translate_attributes(Attributes), ">",
    translate_ast_item(Children, []),
    "</", Name, ">"],
  translate_ast_item(T, [ToAppend | Acc]).

translate_attributes([]) ->
  "";
translate_attributes(Atts) ->
  lists:foldl(fun out_attr/2, [], Atts).
out_attr({Name, Values}, Acc) ->
  [" ", Name, "=\"", string:join(Values, " "), "\"" | Acc].


%% -------------------- Utilities for special handlers -----------------------

search_for_file(File, Path) ->
  Res = os:cmd("find '" ++ Path ++ "' -name '" ++ File ++ "'"),
  string:tokens(Res, "\n").

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
replace_tag([H|T], Search, NewTag, CurrPath, Acc) when is_list(H) ->
  replace_tag(T, Search, NewTag, CurrPath, [H | Acc]);
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
  end.

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
get_tag([], _, _) ->
  undefined;
get_tag([H|T], Search, CurrPath) when is_list(H) ->
  get_tag(T, Search, CurrPath);
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
  end.

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
