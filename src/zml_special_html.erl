-module(zml_special_html).

-export([modify_ast/1]).

modify_ast(InAST, SourceDir, StagingDir) ->
  % TODO:
  %  - add doctype
  %  - preprocess javascript files, combine, and move to staging & AST
  %  - preprocess css files, combine, filter, and inline into AST
  %  - add javascript directive to bottom of AST
  %  - be able to pull in external scripts etc.
  %  - add encoding to meta-tags- default one if there is none specified
  nyi.
