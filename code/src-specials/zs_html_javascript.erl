%% TODO:
%%  - Do nothing when no scripts or magic scripts
%%  - Inline page specific
%%  - Auto detect and load jquery unless prohibited
%%  - Option to turn off inlining page-specific
%%  - Sequential Loader for external(s)
%%  - Option to condense externals
%%  - Option to optimize via clojure (results cached)
%%  - Javascript libraries
%%  - [future] callback to reload page when javascript changes
%%  - [future] allow inline javascript specials?

-module(zs_html_javascript).

-export([process/5]).

process(_ID, Attr, _Children, AST, Options) ->
  MagicFile = zml:find_magic_file(".js", Options),
  Externals = zml:get_attr_vals(script, Attr) ++
    zml:get_attr_vals(scripts, Attr),

  AST.
