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

-define(OPT_ENV(Desc),
        {proplists:get_value(Desc, Options),
         os:getenv(string:to_upper(atom_to_list(Desc)))}).

compile_files() -> compile_files([]).

compile_files([]) ->
  Template = template_stream(standard_io),
  io:format("~s~n", [render(Template)]);

compile_files(FLS) ->
  lists:foreach(fun(FName) ->
    FNameOut = output_file_name(FName),
    io:format("~s --> ~s~n", [FName, FNameOut]),
    Template = template_file(FName),
    ok = file:write_file(FNameOut, render(Template))
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

