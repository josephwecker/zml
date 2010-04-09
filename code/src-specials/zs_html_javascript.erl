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

-define(JS_DEF(M,E,L),
  [{js_magic_file, inline, M},
   {js_externals, external, E},
   {js_libs, inline, L}]).

process(_ID, Attr, _Children, AST, Options) ->
  MagicJS = zml:find_magic_file(".js", Options),
  ExternalJS = zml:get_attr_vals(script, Attr) ++
    zml:get_attr_vals(scripts, Attr),
  LibJS = zml:get_attr_vals(scriptlib, Attr) ++
    zml:get_attr_vals(scriptlibs, Attr) ++
    autojquery(MagicJS),

  InputDef = ?JS_DEF(MagicJS, ExternalJS, LibJS),

  Input = [{K, proplists:get_value(K,Options,DV),In} || {K,DV,In} <- InputDef],

  
  
  Inlines =   [get_inline(K, In) || {K,V,In} <- Instr, V =:= inline, In =!= []],
  Locals =    [get_local(K, In) || {K,V,In} <- Instr, V =:= local, In =!= []],
  Externals = [get_external(K, In) || {K,V,In} <- Instr,V=:=external, In=!=[]],
  
  AST.


% Needs to give the actual javascript
get_inline(js_magic_file, FN) ->
  nyi;
get_inline(js_externals, ExtNames) ->
  nyi;
get_inline(js_libs, Libs) ->
  nyi.

% Needs to get actual file-names of tmp copies
get_local...

% Needs to give the location of the externals
get_external...


