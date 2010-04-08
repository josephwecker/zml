%% Special handler for *table tags in zml
%%
%% Author: Sergei Matusevich <sergei.matusevich@gmail.com>
%%

-module(zml_special_table).

-export([run_handler/5]).

run_handler(ID, Attr, Children, AST, Options) ->
  % io:format("ID=~s Attr=~p Children=~p AST=~p Options=~p~n",
  %           [ID, Attr, Children, AST, Options]),
  AST.
