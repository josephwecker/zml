-module(zml).

-compile(export_all).

compile(InFile) ->
	{ok, CWD} = file:get_cwd(),
	compile(InFile, CWD).

compile(InFile, SpecialDir) ->
	AST = zml_hand_parser:parse(zml_tokenizer:tokenize_file(InFile)),
	SourceDir = filename:dirname(filename:absname(InFile)),
	StagingDirName = ".tmp_" ++ integer_to_list(erlang:phash2(make_ref())),
	{ok, CurrDir} = file:get_cwd(),
	StagingDir = filename:join([CurrDir, StagingDirName]),
	file:make_dir(StagingDir),
	AST2 = run_specialized_handlers(AST, SourceDir, StagingDir, SpecialDir),
	Out = translate_ast_item(AST2, []),
	io:format("~p", [Out]),
	ok = file:write_file(filename:join([StagingDir, "output.html"]), Out),
	StagingDir.

run_specialized_handlers(AST, SourceDir, StagingDir, SpecialDir) ->
	run_spec_handler_inner(AST, SourceDir, StagingDir, SpecialDir, AST).

run_spec_handler_inner([], _, _, _, NewAST) ->
	NewAST;
run_spec_handler_inner([{{Name,ID}, special, Attr, Children} | T],
		DSource, DStage, DSpec, FullAST) ->
	HandlerName = list_to_atom("zml_special_" ++ string:to_lower(Name)),
	case code:ensure_loaded(HandlerName) of
		{module, _} ->
			great;
		_ ->
			HandlerSource = filename:join([DSpec, HandlerName]),
			case code:load_abs(HandlerSource) of
				{module, _} ->
					whew;
				{error, What} ->
					erlang:error([
								"'",What,"' When trying to load handler for ",
								Name," special tag types."])
			end
	end,
	NewAST = HandlerName:run_handler(ID, Attr, Children, FullAST, DSource,
		DStage),
	run_spec_handler_inner(T, DSource, DStage, DSpec, NewAST);
run_spec_handler_inner([H|T],DSource,DStage,DSpec,FullAST) ->
	run_spec_handler_inner(T, DSource, DStage, DSpec, FullAST).


translate_ast_item([], Acc) ->
	lists:reverse(Acc);
translate_ast_item([String | [Next | _] = T], Acc)
when is_list(String) and is_list(Next) ->
	translate_ast_item(T, [" " | [String | Acc]]);
translate_ast_item([String | T], Acc) when is_list(String) ->
	translate_ast_item(T, [String | Acc]);
% In case a special one still remains, remove ID and pretend it's normal
translate_ast_item([{{Name,ID},Type,Attributes,Children} | T], Acc) ->
	translate_ast_item([{Name, Type, Attributes, Children} | T], Acc);
translate_ast_item([{Code,code,[],Children} | T], Acc) ->
	ToAppend = ["!!CODE!!",
		string:join(Code, " "),
		"!!",
		translate_ast_item(Children, []),
		"!!END!!"],
	translate_ast_item(T, [ToAppend | Acc]);
translate_ast_item([{Name,Type,Attributes,[]} | T], Acc) ->
	ToAppend = ["<", Name,
		translate_attributes(Attributes), "/>"],
	translate_ast_item(T, [ToAppend | Acc]);
translate_ast_item([{Name,Type,Attributes,Children} | T], Acc) ->
	ToAppend = [
		"<", Name,
		translate_attributes(Attributes), ">",
		translate_ast_item(Children, []),
		"</", Name, ">"],
	translate_ast_item(T, [ToAppend | Acc]).

translate_attributes([]) ->
	"";
translate_attributes(Atts) when is_tuple(Atts) ->
	lists:foldl(fun out_attr/2, [], dict:to_list(Atts));
translate_attributes(Atts) ->
	lists:foldl(fun out_attr/2, [], Atts).
out_attr({Name, Values}, Acc) ->
	[" ", Name, "=\"", string:join(Values, " "), "\"" | Acc].
