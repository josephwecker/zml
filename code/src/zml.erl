
-module(zml).

% Main function
-export([ compile_static_files/0, compile_static_files/1,
          template_dir/2, template_file/2, template_string/3,
          start/0, render/2, render/3 ]).

-include_lib("kernel/include/file.hrl").


-define(OPT_ENV(Desc),
        {proplists:get_value(Desc, Options),
         os:getenv(string:to_upper(atom_to_list(Desc)))}).

compile_static_files() -> compile_static_files([]).

compile_static_files([]) ->
  {Template, IsStatic} = compile_stream(standard_io, []),
  io:format("~s~n", [
    case IsStatic of
      true  -> Template;
      false -> zml_render:render(Template, fake)
    end]);

compile_static_files(FLS) ->
  lists:foreach(fun(FName) ->
    FNameOut = output_file_name(FName),
    {Template, IsStatic} = compile_file(FName, []),
    io:format("~s --> ~s :: ~s~n", [FName, FNameOut,
      case IsStatic of
        true  -> "static file";
        false -> "dynamic template"
      end]),
    ok = file:write_file(FNameOut,
      case IsStatic of
        true  -> Template;
        false -> zml_render:render(Template, fake)
      end)
  end, FLS).

% TODO: take output path from the options.
output_file_name(FName) ->
  case string:right(FName, 5) of
    [_|".zml"] -> string:left(FName, string:len(FName) - 4);
    _ -> FName
  end ++ ".html".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> ets:new(zml_templates, [set, public, named_table]).

template_dir(Dir, Options) ->
  BaseDir = proplists:get_value(base_dir, Options, "."),
  Path = case Dir of
    "" -> BaseDir;
    _  -> BaseDir ++ "/" ++ Dir
  end,
  case file:list_dir(Path) of
    {ok, Files} ->
      lists:foreach(fun(FName) ->
        case string:right(FName, 5) of
          [_|".zml"] -> template_file(Dir ++ "/" ++ FName, Options);
          _ -> nothing
        end end, Files);
    Err -> Err
  end.

template_file(FName, Options) ->
  BaseDir = proplists:get_value(base_dir, Options, "."),
  Path = BaseDir ++ "/" ++ FName,
  {ok, FileInfo} = file:read_file_info(Path),
  NewTs = FileInfo#file_info.mtime,
  case ets:lookup(zml_templates, FName) of
    [{FName, _Path, Ts, Templ, _IsStatic}] when Ts >= NewTs -> Templ;
    _ -> {NewTempl, IsStatic} = compile_file(Path, Options),
         ets:insert(zml_templates, {FName, Path, NewTs, NewTempl, IsStatic}),
         NewTempl
  end.

template_string(Name, Str, Options) ->
  {Templ, IsStatic} = compile_string(Str, Options),
  ets:insert(zml_templates, {Name, none, 0, Templ, IsStatic}),
  Templ.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @return {ok, ZML template} or atom 'undefined'
% recompile the file, if needed
get_template(Name, Options) ->
  case ets:lookup(zml_templates, Name) of
    [{_Name, none, 0,  Templ, IsStatic}] -> {ok, Templ, IsStatic};
    [{_Name, Path, Ts, Templ, IsStatic}] ->
      {ok, FileInfo} = file:read_file_info(Path),
      NewTs = FileInfo#file_info.mtime,
      case NewTs > Ts of
        true ->
          {NewTempl, IsNewStatic} = compile_file(Path, Options),
          ets:insert(zml_templates, {Name, Path, NewTs, NewTempl, IsNewStatic}),
          {ok, NewTempl, IsNewStatic};
        false ->
          {ok, Templ, IsStatic}
      end;
    [] ->
      case string:right(Name, 5) of
        [_|".zml"] ->  {ok, template_file(Name, Options)};
        _ -> {error, template_not_found, Name}
      end;
    Err -> {error, Err}
  end.

render(Name, Data, Options) ->
  case get_template(Name, Options) of
    {ok, Templ, true } -> Templ;
    {ok, Templ, false} -> zml_render:render(Templ, Data);
    Err -> Err
  end.

render(Name, Options) -> render(Name, fake, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_file(InFile, Options) ->
  SourceFName = filename:absname(InFile),
  Options2 =
    case proplists:lookup(source_filename, Options) of
      none -> [{source_filename, SourceFName} | Options];
      _ -> Options
    end,
  {ok, Bin} = file:read_file(InFile),
  compile_string(binary_to_list(Bin), Options2).

compile_stream(Stream, Options) ->
  Str = io:get_chars(Stream, "", 1024000),
  compile_string(Str, Options).

compile_string(Str, Options) ->
  Options2 = other_options(Options),
  AST = zml_indent:tokenize_string(Str), % TODO: pass options here
  AST2 = run_specialized_handlers(AST, Options2),
  translate_ast_item(AST2, [], true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

other_options(Options) ->
  case ?OPT_ENV(zml_zss_libs) of
    {undefined, V1} when V1 =/= false -> [{zml_zss_libs, V1}];
    _ -> []
  end ++
  case ?OPT_ENV(zml_closure_jar) of
    {undefined, V2} when V2 =/= false -> [{zml_closure_jar, V2}];
    _ -> []
  end ++ Options.

run_specialized_handlers(AST, Options) ->
  run_specialized_handlers_inner(AST, Options, AST).

run_specialized_handlers_inner([], _, NewAST) -> NewAST;

run_specialized_handlers_inner(
    [{{Name, _ID}, special, _Attr, _Children} = Node | T],
    Options, FullAST) ->
  NewAST = zml_util:call_special(Name,
    process_tree, [Node, FullAST, Options], FullAST),
  run_specialized_handlers_inner(T, Options, NewAST);

run_specialized_handlers_inner([_H|T], Options, FullAST) ->
  run_specialized_handlers_inner(T, Options, FullAST).

translate_ast_item([], Acc, IsStatic) -> {lists:reverse(Acc), IsStatic};
translate_ast_item([newline | T], Acc, IsStatic) ->
  translate_ast_item(T, ["\n" | Acc], IsStatic);
translate_ast_item([{var,_} = Var | T], Acc, _IsStatic) ->
  translate_ast_item(T, [Var | Acc], false);
% In case a special one still remains, remove ID and pretend it's normal
translate_ast_item([{{Name, _ID}, Type, Attributes, Children} | T], Acc, IsStatic) ->
  translate_ast_item([{Name, Type, Attributes, Children} | T], Acc, IsStatic);
translate_ast_item([{with, Attr, Children} | T], Acc, _IsStatic) ->
  {ChildAST, _IsStatic} = translate_ast_item(Children, [], true),
   % *with Attr truncated in the special handler:
  translate_ast_item(T, [{with, Attr, ChildAST} | Acc], false);
translate_ast_item([{Name, _Type, Attributes, []} | T], Acc, IsStatic) ->
  {AttrTempl, AttrStatic} = translate_attributes(Attributes, [], true),
  translate_ast_item(T,
    [["<", Name, AttrTempl, "/>"] | Acc], IsStatic and AttrStatic);
translate_ast_item([{Name, _Type, Attributes, Children} | T], Acc, IsStatic) ->
  {AttrTempl,  AttrStatic } = translate_attributes(Attributes, [], true),
  {ChildTempl, ChildStatic} = translate_ast_item(Children, [], true),
  ToAppend = ["<", Name, AttrTempl, ">", ChildTempl, "</", Name, ">"],
  translate_ast_item(T, [ToAppend | Acc], IsStatic and AttrStatic and ChildStatic);
translate_ast_item([Str | T], Acc, IsStatic) ->
  translate_ast_item(T, [Str | Acc], IsStatic).

translate_attributes([], Acc, IsStatic) -> {lists:reverse(Acc), IsStatic};

translate_attributes([{Name, Vals} | Attrs], Acc, IsStatic) ->
  {ValStr, ValStatic} = paste_attr_values(Vals, [], true),
  translate_attributes(Attrs,
    [[" ", Name, "=\"", ValStr, "\""] | Acc], ValStatic and IsStatic).

paste_attr_values([], Acc, IsStatic) -> {lists:reverse(Acc), IsStatic};

paste_attr_values([H | T], [], IsStatic) ->
  paste_attr_values(T, [H], IsStatic andalso not is_tuple(H));

paste_attr_values([H | T], [A | _] = Acc,
    _IsStatic) when is_tuple(H) orelse is_tuple(A) ->
  paste_attr_values(T, [H | Acc], false);

paste_attr_values([H | T], Acc, IsStatic) ->
  paste_attr_values(T, [H, " " | Acc], IsStatic).

