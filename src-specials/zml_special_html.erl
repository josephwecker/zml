%% Special handler for *html tags in zml
%%
%% Author: Joseph Wecker <joseph.wecker@gmail.com>
%%

-module(zml_special_html).

-export([run_handler/6]).

-import(string, [to_lower/1, to_upper/1, join/2]).

-define(TYPES,[
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
    "//<![CDATA[\n",
    "var _zmlll=0,_zmljs=document.createElement('script');_zmljs.src='",
    LibSrc,"';",
    "var head=document.getElementsByTagName('head')[0];",
    "head.appendChild(_zmljs);_zmlw();function _zmlw(){",
    "_zmlll?_zmlc():setTimeout('_zmlw()',150)}function _zmlc(){",
    Inner,"};\n", "//]]>\n"]).

-define(ENC_TOP_X(Enc),
  "<?xml version=\"1.0\" encoding=\"" ++ string:to_upper(Enc) ++ "\"?>\n").

-define(ENCODING_DEFAULT, "utf-8").
-define(LANGUAGE_DEFAULT, "en-us").

metatag(encoding, $x, [Val]) ->
  % Skipping application/xhtml+xml for now
  %"<meta http-equiv=\"content-type\" content=\"application/xhtml+xml; " ++
  "<meta http-equiv=\"content-type\" content=\"text/html; " ++
  "charset="++to_upper(Val)++"\" />";
metatag(encoding, _, [Val]) ->
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=" ++
  to_lower(Val)++"\">";
metatag(language, $x, [Val]) ->
  "<meta http-equiv=\"content-language\" content=\""++to_lower(Val)++"\" />";
metatag(language, _, [Val]) ->
  "<meta http-equiv=\"Content-Language\" content=\""++to_lower(Val)++"\">";
metatag(description, $x, Vals) ->
  "<meta name=\"description\" content=\""++join(Vals," ")++"\" />";
metatag(description, _, Vals) ->
  "<meta name=\"description\" content=\""++join(Vals," ")++"\">";
metatag(keywords, $x, Vals) ->
  "<meta name=\"keywords\" content=\""++join(Vals, " ")++"\" />";
metatag(keywords, _, Vals) ->
  "<meta name=\"keywords\" content=\""++join(Vals, " ")++"\">";
metatag(copyright, $x, Vals) ->
  "<meta name=\"copyright\" content=\"Copyright (c) "++join(Vals," ")++"\" />";
metatag(copyright, _, Vals) ->
  "<meta name=\"copyright\" content=\"Copyright (c) "++join(Vals," ")++"\">";
metatag(nosmarttag, $x, _) ->
  "<meta name=\"MSSmartTagsPreventParsing\" content=\"true\" />";
metatag(nosmarttag, _, _) ->
  "<meta name=\"MSSmartTagsPreventParsing\" content=\"TRUE\">";
metatag(title, _, Vals) ->
  zml:new_tag(title, [], Vals).

run_handler(ID, Attr, Children, FAST, SourceFN, StagingDir) ->
  FAST2 = add_or_replace_doctype(FAST, Attr),
  FAST3 = ensure_head_and_body(ID, Attr, Children, FAST2),
  FAST4 = handle_javascript(ID, Attr, Children, FAST3, SourceFN, StagingDir),
  FAST5 = handle_xhtml(ID, Attr, Children, FAST4),
  FAST6 = handle_zss(ID, Attr, Children, FAST5, SourceFN, StagingDir),
  FAST7 = handle_metas(ID, Attr, FAST6),
  remove_special_attributes(ID, Attr, Children, FAST7).

get_html_attr(Find, Attr, Default) when is_atom(Find) ->
  get_html_attr(atom_to_list(Find), Attr, Default);
get_html_attr(Find, Attr, Default) ->
  case dict:find(Find, Attr) of
    {ok, Val} -> Val;
    error when is_atom(Default) ->
      [atom_to_list(Default)];
    error ->
      [Default]
  end.

pop_html_attr(Find, Attr, Default) when is_atom(Find) ->
  pop_html_attr(atom_to_list(Find), Attr, Default);
pop_html_attr(Find, Attr, Default) ->
  case dict:find(Find, Attr) of
    {ok, Val} ->
      {dict:erase(Find, Attr), Val};
    error when is_atom(Default) ->
      {Attr, [atom_to_list(Default)]};
    error ->
      {Attr, [Default]}
  end.

ensure_head_and_body(ID, Attr, Children, AST) ->
  {AST2, CurrChildren} =
    case zml:get_tag(Children, ["body"]) of
      undefined ->
        RemovedHead = zml:replace_tag(Children, ["head"], []),
        NewChildren = [zml:new_tag(body, normal, dict:new(), RemovedHead)],
        {zml:replace_tag(AST, [{"html", ID}],
            zml:new_tag({"html", ID}, special, Attr, NewChildren)),
          NewChildren};
      _ ->
        {AST, Children}
    end,
  case zml:get_tag(Children, ["head"]) of % Looking in old children here
    undefined ->
      zml:replace_tag(AST2, [{"html", ID}],
        zml:new_tag({"html", ID}, special, Attr,
          [zml:new_tag("head",normal,dict:new(),[]) | CurrChildren]));
    ExistingHead ->
      zml:replace_tag(AST2, [{"html", ID}],
        zml:new_tag({"html", ID}, special, Attr,
          [ExistingHead | CurrChildren]))
  end.

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
  [Type] = get_html_attr(type, Attr, ?DEFAULT_TYPE),
  DoctypeString =
    case proplists:get_value(list_to_atom(Type), ?TYPES) of
      undefined ->
        Allowed = string:join(lists:map(
            fun atom_to_list/1,
            proplists:get_keys(?TYPES)),", "),
        erlang:error("'" ++ Type ++
          "' html type unknown. Try one of: " ++ Allowed);
      DS -> DS
    end,
  [DoctypeString | AST].

handle_zss(ID, Attr, Children, AST, SourceFN, {_, DTmp, _, _DCSS, _, _}) ->
  NormAttr = attribute_alias(Attr, "stylesheet", "stylesheets"),
  Scripts = get_aux_files(SourceFN, ".zml", ".zss", NormAttr, "stylesheets"),
  CSS = lists:foldl(fun(A,Acc) -> process_zss(AST,A,Acc,DTmp) end, [], Scripts),
  io:format("~p", [CSS]),
  AST.

process_zss(AST, FName, Acc, DTmp) ->
  TmpFN = filename:join([DTmp, zml:tmp_filename()]),
  case zml:pull_in_file(FName, TmpFN) of
    ok ->
      ZSS_AST = zss:compile(TmpFN),
      Culled = remove_unused_css(ZSS_AST, AST, []),
      Normalized = zss_parser:combine_dups(Culled),
      io:format("~n--->Culled<----~n~p~n", [Normalized]),
      %CSS = zss:output_css(Culled),
      %Acc ++ "~n" ++ CSS;
      Acc;
    {error, Reason} ->
      erlang:error(["Couldn't get ZSS file ", FName, Reason])
  end.

remove_unused_css([], _AST, Acc) ->
  lists:reverse(Acc);
remove_unused_css([{Selectors, Atts} | T], AST, Acc) ->
  ActiveSelectors = lists:reverse(lists:foldl(
    fun(A,InAcc) -> 
        case ast_has_selector(AST,A) of
          true -> [A | InAcc];
          false -> InAcc
        end
    end, [], Selectors)),
  case ActiveSelectors of
    [] ->
      remove_unused_css(T, AST, Acc);
    _ ->
      remove_unused_css(T, AST, [{ActiveSelectors, Atts} | Acc])
  end.

%% OK if it has some false positives, so we can keep it simple.
%% Get rid of/ignore ":...", "[...]", and turn ">" into " ", and finally, "+"
%% is pretty much ignored- returns true for now.
%% *
%% E
%% E F
%% Of course all with optional # and .
ast_has_selector(AST, Sel) ->
  % All that work not using regex earlier and now resorting to it here.  *sigh*
  % TODO: replace regex w/ some simple transforms
  S2 = re:replace(Sel, ":[^\\s]*", "", [{return, list}, global]),
  S3 = re:replace(S2,  "\\[.*?\\]","", [{return, list}, global]),
  S4 = re:replace(S3,  "\\+.*","", [{return, list}, global]),
  SFin = re:replace(S4,  ">"," ", [{return, list}, global]),

  case string:chr(SFin, $+) of
    0 -> 
      Elements = string:tokens(string:strip(SFin), " "),
      scan_ast(Elements, AST);
    _ ->
      true
  end.

scan_ast([], _) ->
  true;
scan_ast(_, []) ->
  false;
scan_ast(Elems, [Txt | T]) when is_list(Txt) ->
  scan_ast(Elems, T);
scan_ast([E | T] = Elems, [{NEl, _, NAtt, NChildren}  | TAST]) ->
  case elements_match(E, NEl, NAtt) of
    true ->
      case T of
        [] ->
          true;
        _ ->
          scan_ast(T, NChildren)
      end;
    false ->
      case scan_ast(Elems, NChildren) of
        true ->
          true;
        false ->
          scan_ast(Elems, TAST)
      end
  end.

elements_match(ElStr, {Name, _ID}, Attrs) ->
  elements_match(ElStr, Name, Attrs);
elements_match(ElStr, Name, Attrs) ->
  {Element, Classes, ID} = pull_selector_atts(ElStr),
  ElementMatch =
    case Element of
      "*" ->
        true;
      _ ->
        Element == Name
    end,
  case ElementMatch of
    false -> false;
    true ->
      att_includes(Attrs, "class", Classes) and
        att_includes(Attrs, "id", ID)
  end.

att_includes(_Attr, _Key, []) ->
  true;
att_includes(Attr, Key, ReqVals) ->
  case dict:find(Key, Attr) of
    error ->
      false;
    {ok, HasVals} ->
      lists:all(fun(Req) -> lists:member(Req, HasVals) end, ReqVals)
  end.

% Given a string with optional element and strung classes, id markers, it
% returns the element name (* if none specified), the list of classes, and the
% list of IDs (even though everyone knows you can't have that...)
pull_selector_atts(E) ->
  pull_selector_atts(E, elem, [], [], [], []).

pull_selector_atts([], elem, [], [], [], []) ->
  {"*", [], []};
pull_selector_atts([], elem, TAcc, [], [], []) ->
  {lists:reverse(TAcc), [], []};
pull_selector_atts([], class, TAcc, El, ClAcc, IDAcc) ->
  {El, [lists:reverse(TAcc) | ClAcc], IDAcc};
pull_selector_atts([], id, TAcc, El, ClAcc, IDAcc) ->
  {El, ClAcc, [lists:reverse(TAcc) | IDAcc]};

pull_selector_atts([$.|T], elem, [], [], [], []) ->
  pull_selector_atts(T, class, [], "*", [], []);
pull_selector_atts([$.|T], elem, TmpAcc, [], [], []) ->
  pull_selector_atts(T, class, [], lists:reverse(TmpAcc), [], []);
pull_selector_atts([$.|T], class, TmpAcc, El, ClAcc, IDAcc) ->
  pull_selector_atts(T, class, [], El, [lists:reverse(TmpAcc) | ClAcc], IDAcc);
pull_selector_atts([$.|T], id, TmpAcc, El, ClAcc, IDAcc) ->
  pull_selector_atts(T, class, [], El, ClAcc, [lists:reverse(TmpAcc) | IDAcc]);

pull_selector_atts([$#|T], elem, [], [], [], []) ->
  pull_selector_atts(T, id, [], "*", [], []);
pull_selector_atts([$#|T], elem, TmpAcc, [], [], []) ->
  pull_selector_atts(T, id, [], lists:reverse(TmpAcc), [], []);
pull_selector_atts([$#|T], class, TmpAcc, El, ClAcc, IDAcc) ->
  pull_selector_atts(T, id, [], El, [lists:reverse(TmpAcc) | ClAcc], IDAcc);
pull_selector_atts([$#|T], id, TmpAcc, El, ClAcc, IDAcc) ->
  pull_selector_atts(T, id, [], El, ClAcc, [lists:reverse(TmpAcc) | IDAcc]);

pull_selector_atts([H|T], Type, TmpAcc, El, ClAcc, IDAcc) ->
  pull_selector_atts(T, Type, [H | TmpAcc], El, ClAcc, IDAcc).


handle_metas(ID, Attr, AST) ->
  {_,_,_,Children} = zml:get_tag(AST, [{"html",ID}]),
  [[Tp | _]] = get_html_attr(type, Attr, ?DEFAULT_TYPE), % ignore all but first specified

  {NewAttr, Metas} = lists:foldr(fun new_metas/2, {Attr, []}, [
      {encoding,    Tp, ?ENCODING_DEFAULT},
      {language,    Tp, ?LANGUAGE_DEFAULT},
      {description, Tp, none},
      {keywords,    Tp, none},
      {copyright,   Tp, none},
      {nosmarttag,  Tp, true},
      {title,       Tp, none}]),
  {_,_,HAttr,HChildren} = zml:get_tag(Children, ["head"]),
  NewHead = zml:new_tag("head", normal, HAttr, HChildren ++ Metas),
  NewChildren = zml:replace_tag(Children, ["head"], NewHead),
  NewFull = zml:new_tag({"html",ID}, special, NewAttr, NewChildren),
  zml:replace_tag(AST, [{"html",ID}], NewFull).

new_metas({Name, Type, Def}, {Attr, Acc}) ->
  case pop_html_attr(Name, Attr, Def) of
    {NewAttr, ["none"]} -> {NewAttr, Acc};
    {NewAttr, Val} -> {NewAttr, [metatag(Name, Type, Val) | Acc]}
  end.

remove_special_attributes(ID, Attr, Children, AST) ->
  CleanAttrs = lists:foldl(fun dict:erase/2, Attr,
    ["script", "scripts", "stylesheet", "stylesheets", "type", "encoding", "title"]),
  NewFull = zml:new_tag({"html",ID}, special, CleanAttrs, Children),
  zml:replace_tag(AST, [{"html",ID}], NewFull).
      
handle_xhtml(_ID, Attr, _Children, AST) ->
  % TODO: html namespace and language attribute
  [TypeFC | _] = get_html_attr(type, Attr, ?DEFAULT_TYPE),

  case true of
    true -> % Skipping xml prolog for now
      AST;
    false ->
      Encoding = get_html_attr(encoding, Attr, ?ENCODING_DEFAULT),
      case TypeFC == $x of
        true ->
          [?ENC_TOP_X(Encoding) | AST];
        false ->
          AST
      end
  end.

attribute_alias(Attr, From, To) ->
  case dict:find(From, Attr) of
    {ok, Val} ->
      D2 = dict:erase(From, Attr),
      dict:store(To, Val, D2);
    _ -> Attr
  end.

handle_javascript(ID, Attr, Children, AST, SourceFN, {_, DTmp, DJS, _, _, _}) ->
  NormAttr = attribute_alias(Attr, "script", "scripts"),

  case get_js_list(NormAttr, SourceFN) of
    {[],[]} ->
      AST;
    {LibFiles, IndFiles} ->
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
      Inline = lists:flatten(?JS_LOADER("js/" ++ MD5Sum ++ ".js",
          binary_to_list(Inner))),

      % Put inline script in body
      ScriptTag = zml:new_tag(script, [{"type", ["text/javascript"]}], [Inline]),
      {"body", normal, BAttr, BChildren} = zml:get_tag(Children, ["body"]),
      Children2 = zml:replace_tag(Children, ["body"],
        zml:new_tag(body, normal, BAttr, BChildren ++ [ScriptTag])),

      % Remove scripts from html parameters
      NewAttr = dict:erase("scripts", NormAttr),
      zml:replace_tag(AST, [{"html", ID}],
        zml:new_tag({"html", ID}, special, NewAttr, Children2))
  end.

% Look in the attributes and in the source directory to see which javascript
% files should be associated with this html block.  Resolves paths relative to
% the source file.
get_js_list(Attr, Source) ->
  AllFilesAbs = get_aux_files(Source, ".zml", ".js", Attr, "scripts"),
  BaseName = filename:basename(Source, ".zml"),
  lists:partition(fun(FN) -> filename:basename(FN, ".js") /= BaseName end,
    AllFilesAbs).

get_aux_files(Src, SrcExt, FindExt, Attr, Key) ->
  Dir = filename:dirname(Src),
  BaseName = filename:basename(Src, SrcExt),
  TwinFile = zml:search_for_file(BaseName ++ FindExt, Dir),
  AllFiles =
    case dict:find(Key, Attr) of
      error -> TwinFile;
      {ok, Files} -> Files ++ TwinFile
    end,
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
    end, AllFiles).

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
