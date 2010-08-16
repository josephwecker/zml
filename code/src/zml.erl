
-module(zml).

% Main function
-export([ compile_static_files/0, compile_static_files/1,
          template_dir/2, template_file/3, template_string/3,
          start/0, render/3 ]).

-include_lib("kernel/include/file.hrl").


-define(OPT_ENV(Desc),
        {proplists:get_value(Desc, Options),
         os:getenv(string:to_upper(atom_to_list(Desc)))}).

compile_static_files() -> compile_static_files([]).

compile_static_files([]) ->
  Template = compile_stream(standard_io, []),
  io:format("~s~n", [zml_render:render(Template, fake)]);

compile_static_files(FLS) ->
  lists:foreach(fun(FName) ->
    FNameOut = output_file_name(FName),
    io:format("~s --> ~s~n", [FName, FNameOut]),
    Template = compile_file(FName, []),
    ok = file:write_file(FNameOut, zml_render:render(Template, fake))
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
  case file:list_dir(Dir) of
    {ok, Files} ->
      lists:foreach(fun(FName) ->
        case string:right(FName, 5) of
          [_|".zml"] -> template_file(
            list_to_atom(string:left(FName, string:len(FName) - 4)),
            Dir ++ "/" ++ FName, Options);
          _ -> nothing
        end end, Files);
    Err -> Err
  end.

template_file(Id, Path, Options) ->
  {ok, FileInfo} = file:read_file_info(Path),
  NewTs = FileInfo#file_info.mtime,
  case ets:lookup(zml_templates, Id) of
    [{_Id, _Path, Ts, _Templ}] when Ts >= NewTs -> ok;
    _ -> ets:insert(zml_templates,
      {Id, Path, NewTs, compile_file(Path, Options)})
  end.

template_string(Id, Str, Options) ->
  ets:insert(zml_templates, {Id, none, 0, compile_string(Str, Options)}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @return {ok, ZML template} or atom 'undefined'
% recompile the file, if needed
get_template(Id, Options) ->
  case ets:lookup(zml_templates, Id) of
    [{_Id, none, 0,  Templ}] -> Templ;
    [{_Id, Path, Ts, Templ}] ->
      {ok, FileInfo} = file:read_file_info(Path),
      NewTs = FileInfo#file_info.mtime,
      {ok, case NewTs > Ts of
        true  -> NewTempl = compile_file(Path, Options),
                 ets:insert(zml_templates, {Id, Path, NewTs, NewTempl}),
                 NewTempl;
        false -> Templ
      end};
    _ -> undefined
  end.

render(Id, Data, Options) ->
  case get_template(Id, Options) of
    {ok, Templ} -> zml_render:render(Templ, Data);
    Err -> Err
  end.

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
  translate_ast_item(AST2, []).

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

translate_attributes(Attrs) ->
  [ [" ", Name, "=\"", zml_util:intersperse(Values, " "), "\""]
    || {Name, Values} <- Attrs ].

