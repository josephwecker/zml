%% Special handler for *html tags in zml
%%
%% Author: Joseph Wecker <joseph.wecker@gmail.com>
%%
%% TODO (Bugs and Polish):
%%  - Put js loader etc. in correct spot in AST
%%  - Simplified case for loader if inline JS is empty
%%  - Check environment variable for closure jar before trying the command in
%%    order to make the error less cryptic.
%%  - Shorten the library js filename - look in that dir. and see what's
%%    already there, etc.?
%%
%% TODO Functionality:
%%  - Make sure that there is a head and body- insert empty ones if not.
%%

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

-define(JS_LOADER(LibSrc, Inner), [
		"var _zmlll=0,_zmljs=document.createElement('script');_zmljs.src='",
		LibSrc,"';",
		"var head=document.getElementsByTagName('head')[0];",
		"head.appendChild(_zmljs);_zmlw();function _zmlw(){",
		"_zmlll?_zmlc():setTimeout('_zmlw()',150)}function _zmlc(){",
		Inner,"};"]).

run_handler(ID, Attr, Children, FAST, SourceFN, StagingDir) ->
	% TODO:
	%  - preprocess javascript files, combine, and move to staging & AST
	%  - preprocess css files, combine, filter, and inline into AST
	%  - add javascript directive to bottom of AST
	%  - be able to pull in external scripts etc.
	%  - add encoding to meta-tags- default one if there is none specified
	%  - pull in and preprocess all images
	FAST2 = add_or_replace_doctype(FAST, Attr),
	FAST3 = handle_javascript(ID, Attr, Children, FAST2, SourceFN, StagingDir),
	FAST3.

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

handle_javascript(ID, Attr, Children, AST, SourceFN, {_, DTmp, DJS, _, _, _}) ->
	{LibFiles, IndFiles} = get_js_list(Attr, SourceFN),

	% Optimize "lib" js files
	LoadedFiles = load_js_files(LibFiles, DTmp),
	OptLibFName = filename:join([DTmp, zml:tmp_filename()]),
	optimize_js(LoadedFiles, OptLibFName),
	file:write_file(OptLibFName, "_zmlll=1;", [append]),
	[MD5Sum | _] = string:tokens(os:cmd("md5sum '"++OptLibFName++"'")," "),
	FinLibFName = filename:join([DJS, MD5Sum ++ ".js"]),
	file:rename(OptLibFName, FinLibFName),

	% Optimize inline js
	LF2 = load_js_files(IndFiles, DTmp),
	OptIndFName = filename:join([DTmp, zml:tmp_filename()]),
	optimize_js(LF2, OptIndFName, false),
	{ok, Inner} = file:read_file(OptIndFName),
	Inline = ?JS_LOADER("js/" ++ MD5Sum ++ ".js", Inner),

  NewAttr = dict:erase("script", Attr),
  NewChildren = Children ++
    [zml:new_tag(script, [{type, "text/javascript"}], [Inline])],

  AST2 = zml:alter_first(AST, {"html", ID}, NewAttr, NewChildren),
	AST2.

% Look in the attributes and in the source directory to see which javascript
% files should be associated with this html block.  Resolves paths relative to
% the source file.
get_js_list(Attr, Source) ->
	Dir = filename:dirname(Source),
	BaseName = filename:basename(Source, ".zml"),
	TwinJSFiles = zml:search_for_file(BaseName ++ ".js", Dir),
	AllFiles =
		case dict:find("script", Attr) of
			error -> TwinJSFiles;
			{ok, Scripts} -> Scripts ++ TwinJSFiles
		end,
	AllFilesAbs =
		lists:map(
			fun([First | _T] = FName) ->
				case First of
					$/ -> FName;
					_ ->
						case string:str(FName, "://") of
							0 -> filename:join([Dir, FName]);
							_ -> FName
						end
				end
			end, AllFiles),
	lists:partition(fun(FN) -> filename:basename(FN, ".js") /= BaseName end,
		AllFilesAbs).

% Take the potential list of javascript files and load them into a temporary
% staging directory.  Ignore duplicate files.  Pull any remote files.
% Ignores dups first by exact filename/path, and then by md5sum.
load_js_files(JSList, DTmp) ->
	load_js_files(JSList, DTmp, dict:new(), []).
load_js_files([], _, _, Loaded) ->
	lists:reverse(Loaded);
load_js_files([Try | T], DTmp, LoadedKeys, Loaded) ->
	case dict:find("FN" ++ Try, LoadedKeys) of
		{ok, _} ->
			load_js_files(T, DTmp, LoadedKeys, Loaded);
		error ->
			DestName = filename:join([DTmp, zml:tmp_filename()]),
			case zml:pull_in_file(Try, DestName) of
				ok ->
					[MD5Sum | _Junk] = string:tokens(
						os:cmd("md5sum '" ++ DestName ++ "'"), " "),
					case dict:find("MD5" ++ MD5Sum, LoadedKeys) of
						{ok, _} ->
							ok = file:delete(DestName),
							load_js_files(T, DTmp, LoadedKeys, Loaded);
						error ->
							D2 = dict:store("FN" ++ Try, true, LoadedKeys),
							D3 = dict:store("MD5"++MD5Sum, true, D2),
							load_js_files(T, DTmp, D3, [DestName | Loaded])
					end;
				{error, Reason} ->
					erlang:error(["Couldn't copy JS file ", Try, Reason])
			end
	end.

optimize_js(Files, Dest) ->
	optimize_js(Files, Dest, false).

optimize_js([], _, _) ->
	[];
optimize_js(Files, Dest, Advanced) ->
	Cmd = lists:flatten(["java -jar $ZML_CLOSURE_JAR",
		lists:map(fun(A) -> [" --js=",A] end, Files),
		" --js_output_file=", Dest,
		case Advanced of
			true -> " --compilation_level=ADVANCED_OPTIMIZATIONS";
			false -> ""
		end,
		" --warning_level=QUIET"]),
	case string:strip(os:cmd(Cmd)) of
		[] -> ok;
		Result -> erlang:error({"Javascript Error", Result})
	end.
