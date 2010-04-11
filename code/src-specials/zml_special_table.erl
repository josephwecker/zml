%% Special handler for *table tags in zml
%%
%% Author: Sergei Matusevich <sergei.matusevich@gmail.com>
%%

-module(zml_special_table).

-export([run_handler/5]).


run_handler(ID, Attr, Children, AST, Options) ->
  io:format("ID=~s Attr=~p Children=~p AST=~p Options=~p~n",
            [ID, Attr, Children, AST, Options]),
  Table = tr(Children, []),
  zml:update_tag(AST, {"table", ID}, special, Attr, Table).


tr([], Acc) -> lists:reverse(Acc);

tr([newline | T], Acc) -> tr(T, Acc);

tr(["|" | T], Acc) ->
  {Tds, Rest} = td(T, [], []),
  tr(Rest, [zml:new_tag(tr, [], Tds) | Acc]);

tr([H | _], _Acc) -> erlang:error(
  "Table line must start with '|', got '" ++ H ++ "' instead.").


td(["|" | T], Acc, Tds) ->
  td(T, [], [zml:new_tag(td, [], lists:reverse(Acc)) | Tds]);

td([newline | T], [], Tds) -> {lists:reverse(Tds), T};

td([newline | _], _, _) -> erlang:error("Table line must end with '|'.");

td([], [], Tds) -> {lists:reverse(Tds), []};

td([], _, _) -> erlang:error("Table line must end with '|'.");

td([H | T], Acc, Tds) -> td(T, [H | Acc], Tds).

