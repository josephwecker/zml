%% TODO:
%%   - (Future) build list of images from culled zss and do image processing
 
-module(zs_html_zss_images).

-export([process/5]).
-include("zml_special_html.hrl").

process(ID, Attr, _Children, AST, Options) ->
  DeclaredZSS = get_declared_zss(Attr, Options),
  Processed = lists:filter(
    fun is_tuple/1,
    [process_styles(Styles, AST, Options) || Styles <- DeclaredZSS]),
  Rendered = "\n" ++ lists:flatten(lists:map(fun({T, CSS}) ->
          {Prepend, Append} = proplists:get_value(T, ?STYLESHEET_TAGS),
          [Prepend, CSS, Append, "\n"]
      end, Processed)),
  zml:append_children(AST, [{"html",ID},"head"], [Rendered]).

get_declared_zss(Attr, Options) ->
  % Explicitly declared
  Search = zml:get_search_paths(Options),
  Declared = lists:map(fun({Type, _Tags}) ->
        Given = zml:get_attr_vals(Type, Attr) ++
                zml:get_attr_vals(Type ++ "s", Attr),
        Found = [find_file(F, ".zss", Search) || F <- Given],
        {Type, [Abs || {ok, Abs} <- Found]}
    end, ?STYLESHEET_TAGS),
  % Look for a magic one as well
  Declared2 =
    case proplists:get_value(source_filename, Options) of
      undefined -> Declared;
      SFN ->
        case find_magic_file(SFN, Options) of
          none -> Declared;
          MagicFile -> zml:append_attr(Declared, {"style", [MagicFile]})
        end
    end,
  % And full libraries
  case zml:get_attr_vals(stylelib, Attr) ++ zml:get_attr_vals(stylelibs, Attr) of
    [] -> Declared2;
    Libs ->
      append_lib_styles(Options, Declared2, Libs)
  end.

append_lib_styles(_Opts, Dec, []) ->
  Dec;
append_lib_styles(Opts, Dec, [Lib | T]) ->
  {ok, Dir} = find_lib_dir(Lib, Opts),
  Dec2 = lists:foldl(fun({Type,_Tags}, Acc) ->
      FName = filename:join([Dir, Type ++ ".zss"]),
      case filelib:is_file(FName) of
        true ->
          zml:append_attr(Acc, {Type, [FName]});
        false ->
          Acc
      end
    end, Dec, ?STYLESHEET_TAGS),
  append_lib_styles(Opts, Dec2, T).

find_lib_dir(Wanted, Opts) ->
  case proplists:get_value(zml_zss_libs, Opts) of
    undefined -> {error, "zml_zss_libs undefined"};
    RawLibs ->
      Libs = string:tokens(RawLibs, ";,"),
      case lists:dropwhile(fun(E) -> dir_missing_wanted(E,Wanted) end, Libs) of
        [] -> {error, "ZSS library not found"};
        [Found | _] -> {ok, filename:join([Found, Wanted])}
      end
  end.

dir_missing_wanted(TryBaseDir, Lookfor) ->
  TryDir = filename:join([TryBaseDir, Lookfor]),
  not filelib:is_dir(TryDir).

% Give back: {Type, InlineCSS}
process_styles({Type, Sheets}, AST, _Options) ->
  case [remove_unused_css(zss:compile(ZSSF), AST, []) || ZSSF <- Sheets] of
    [] ->
      empty;
    ZSSTrees ->
      {Type, zss:output_css(zss_parser:combine_dups(lists:flatten(ZSSTrees)))}
  end.

find_magic_file(SourceName, Options) ->
  BaseName = filename:rootname(filename:basename(SourceName)),
  case find_file(BaseName, ".zss", zml:get_search_paths(Options)) of
    {ok, FullName} -> FullName;
    _ -> none
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
%% Of course all with optional # and .  Also handle *.
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
  case zml:get_attr_vals(Key, Attr) of
    [] ->
      false;
    HasVals ->
      lists:all(fun(Req) -> lists:member(Req, HasVals) end, ReqVals)
  end.

% Given a string with optional element and strung classes, id markers, it
% returns the element name (* if none specified), the list of classes, and the
% list of IDs (even though everyone knows you can't have that...)
% That is, a simple, naive css rule parser
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

