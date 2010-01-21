-module(zml_special_html).

-export([run_handler/6]).

-define(TYPES,
	[
		{strict,
			"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"" ++
			" \"http://www.w3.org/TR/html4/strict.dtd\">\n"},
		{transitional,
			"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"" ++
			" \"http://www.w3.org/TR/html4/loose.dtd\">\n"},
		{frameset,
			"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"" ++
			" \"http://www.w3.org/TR/html4/frameset.dtd\">\n"},
		{xhtml_strict,
			"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"" ++
			" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"},
		{xhtml_transitional,
			"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"" ++
			" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"},
		{xhtml_frameset,
			"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"" ++
			" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">\n"},
		{html3,
			"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"},
		{html2,
			"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n"}]).

-define(DEFAULT_TYPE, xhtml_strict).

run_handler(ID, Attr, Children, FAST, SourceDir, StagingDir) ->
	% TODO:
	%  - add doctype
	%  - preprocess javascript files, combine, and move to staging & AST
	%  - preprocess css files, combine, filter, and inline into AST
	%  - add javascript directive to bottom of AST
	%  - be able to pull in external scripts etc.
	%  - add encoding to meta-tags- default one if there is none specified
	FAST2 = add_or_replace_doctype(FAST, Attr),
	FAST2.

add_or_replace_doctype(AST, Attr) ->
	[FirstLine | _] = AST,
	case is_list(FirstLine) of
		true ->
			case (string:sub_word(FirstLine,1) == "<!DOCTYPE") of
				true -> erlang:error("Please do not declare a DOCTYPE- use a " ++ 
						"'type' attribute on the *html tag instead.");
				false -> ok
			end;
		_ -> ok
	end,
	Type =
		case dict:find("type", Attr) of
			{ok, [Val]} -> list_to_atom(string:to_lower(Val));
			error -> ?DEFAULT_TYPE
		end,
	DoctypeString =
		case proplists:get_value(Type, ?TYPES) of
			undefined ->
				Allowed = string:join(lists:map(
						fun atom_to_list/1,
						proplists:get_keys(?TYPES)),", "),
				erlang:error("'" ++ atom_to_list(Type) ++
					"' html type unknown. Try one of: " ++ Allowed);
			DS -> DS
		end,
	[DoctypeString | AST].


