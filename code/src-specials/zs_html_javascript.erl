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

process(_ID, Attr, _Children, AST, Options) ->
  MagicJS = zml:find_magic_file(".js", Options),
  ExternalJS = zml:get_attr_vals(script, Attr) ++
    zml:get_attr_vals(scripts, Attr),
  LibJS = zml:get_attr_vals(scriptlib, Attr) ++
    zml:gett_attr_vals(scriptlibs, Attr) ++
    autojquery(MagicJS),
  
  Inline = get_all_inline(MagicJS, ExternalJS, LibJS, Options),
  Local = process_locals(MagicJS, ExternalJS, LibJS, Options),
  External = get_externals(MagicJS, ExternalJS, LibJS, Options),

  
  AST.
