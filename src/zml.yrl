
% PROBLEMS:
%  No newline tokens so it doesn't do sibling tags
%  Isn't really parsing attributes
%  Isn't figuring out tag types
%  No mechanism for inline tags

Nonterminals tags tag attributes words child children.
Terminals indent word dedent start_attrs finish_attrs.
Rootsymbol tags.

tags -> tags tag : '$1' ++ ['$2'].
tags -> tag tag : ['$1', '$2'].

words -> words word : '$1' ++ ['$2'].
words -> word word : ['$1', '$2'].
attributes -> start_attrs word finish_attrs : value_of('$2').
attributes -> start_attrs words finish_attrs : '$2'.

tag -> indent word attributes dedent : {value_of('$2'), '$3', []}.
tag -> indent word dedent : {value_of('$2'), [], []}.
tag -> indent word attributes child dedent : {value_of('$2'), '$3', ['$4']}.
tag -> indent word attributes children dedent : {value_of('$2'), '$3', '$4'}.
tag -> indent word child dedent : {value_of('$2'), [], ['$3']}.
tag -> indent word children dedent : {value_of('$2'), [], '$3'}.

child -> tag : '$1'.
child -> word: value_of('$1').

children -> child child : ['$1', '$2'].
children -> children child : '$1' ++ ['$2'].

Erlang code.
value_of(Token) ->
    element(3, Token).
