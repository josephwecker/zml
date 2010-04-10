%% Javascript portion of the "html" special handler

%% Options (displayed with default)
%%
%%  The following can have one of 'inline', 'local', or 'external'
%%   inline ->   Going to be in your html document
%%   local ->    Actually pull in the files and deploy them (sometimes after
%%               running optimizations etc. over them)
%%   external -> Have a link to it and load it in normally
%%
%%  {js_magic_file,         inline}
%%  {js_externals,          external}
%%  {js_libs,               inline}
%%  
%%  {js_autoload_jquery,    true}  % simply adds a library entry before any other processing
%%  {js_parallel_load,      true}
%%  {js_load_at_bottom,     true}
%%  
%%  {js_script_dir,         "./scripts"}
%%
%%  (The last three require a script_dir and only work on "local")
%%  {js_condense,           false}
%%  {js_minify,             false}
%%  {js_optimize,           false}
%%
%% Basic flow
%% -------------
%%  * gather inlines vs. local vs. externals
%%  * local stuff processed and stored
%%  * inlines, local output file(s), and externals combined according to
%%    js_parallel_load and js_load_at_bottom
%%

-module(zs_html_javascript).

-export([process/5]).
-include("zml_special_html.hrl").

-define(JS_DEF(M,E,L),
  [{js_magic_file, inline, M},
   {js_externals, external, E},
   {js_libs, inline, L}]).

process(ID, Attr, _Children, AST, Options) ->
  MagicJS = zml:find_magic_file(".js", Options),
  ExternalJS = zml:get_attr_vals(script, Attr) ++
    zml:get_attr_vals(scripts, Attr),
  LibJS = zml:get_attr_vals(scriptlib, Attr) ++
    zml:get_attr_vals(scriptlibs, Attr) ++
    autojquery(MagicJS),

  InputDef = ?JS_DEF(MagicJS, ExternalJS, LibJS),
  Input = [{K, proplists:get_value(K,Options,DV),In} || {K,DV,In} <- InputDef],
  Search = zml:get_search_paths(Options),
  
  Inlines =   [get_inline(K, In, Search) || {K,V,In} <- Input, V =:= inline, In=/= []],
%  Locals =    [get_local(K, In) || {K,V,In} <- Instr, V =:= local, In =!= []],
  Locals = [],
%  Externals = [get_external(K, In) || {K,V,In} <- Instr,V=:=external, In=!=[]],
  Externals = [],

  ScriptSection =
    create_script_part(Inlines, Locals ++ Externals,
      proplists:get_value(js_parallel_load, Options, true)),
  AddTo =
    case proplists:get_value(js_load_at_bottom, Options, true) of
      true -> "body";
      false -> "head"
    end,
  case ScriptSection of
    none -> AST;
    _ -> zml:append_children(AST, [{"html",ID}, AddTo], ScriptSection)
  end.

autojquery(_JSFile) ->
  [].

% Needs to give the actual javascript
get_inline(js_magic_file, FN, _) ->
  {ok, Res} = file:read_file(FN),
  Res;
get_inline(js_externals, ExtNames, Search) ->
  % TODO: warnings on error instead of ignoring
  SearchRes = [zml:find_file(N, Search) || N <- ExtNames],
  [file:read_file(FN) || {ok, FN} <- SearchRes];
get_inline(js_libs, _Libs, _Search) ->
  % TODO: implement
  [].

% Needs to get actual file-names of tmp copies
%get_local...

% Needs to give the location of the externals
%get_external(js_magic_file, 

create_script_part([], [], _) ->
  none;
create_script_part(Inline, [], _) ->
  % No need for special loading code
  [zml:new_tag(script,
    [{"type", ["text/javascript"]}],
    [?JS_START, Inline, ?JS_END])];

create_script_part([], External, false) ->
  % No need for inline or parallel- standard ol' script tags
  [generic_script_tag(Src) || Src <- External];
create_script_part([], _External, true) ->
  % No need for delaying and loading inline, but load in parallel
  % TODO: You are here!

  [].

generic_script_tag(Src) ->
  zml:new_tag(script, [{"type", ["text/javascript"]}, {"src", [Src]}], [""]).
