%% Takes a token string from zml_tokenizer and turns it into a parsed tree.
%%
%% AUTHOR: Joseph Wecker <joseph.wecker@gmail.com>
%% 
%% TODO:
%%
%%

-module(zml_parser).

-compile(export_all).


build_tree(TokenList) ->
	build_tree(TokenList, []).

build_tree(TokenList, CurrLvl) ->
	nyi.
