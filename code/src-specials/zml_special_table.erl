%% Special handler for *table tags in zml
%%
%% Author: Sergei Matusevich <sergei.matusevich@gmail.com>
%%

-module(zml_special_table).

-export([run_handler/5]).


run_handler(ID, Attr, Children, AST, _Options) ->
  {[AttrTR, AttrTD], AttrTable} = extract_attrs(["tr_", "td_"], Attr),
  Table = tr(Children, AttrTR, AttrTD, []),
  zml:update_tag(AST, {"table", ID}, special, AttrTable, Table).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_attrs(Prefixes, Attrs) ->
  extract_attrs(Prefixes, Attrs,
    lists:map(fun(_) -> [] end, Prefixes), []).

extract_attrs(_Prefixes, [], Acc, Rest) ->
  {Acc, lists:reverse(Rest)};

extract_attrs(Prefixes, [H | T], Acc, Rest) ->
  {NewAcc, Del} = extract_one_attr(Prefixes, H, Acc, [], false),
  extract_attrs(Prefixes, T, NewAcc,
    case Del of true -> Rest; _ -> [H | Rest] end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_one_attr([Prefix | PT], {Key, Val}, [Acc | AT], Res, Found) ->
  IsPrefix = lists:prefix(Prefix, Key),
  extract_one_attr(PT, {Key, Val}, AT, [
    case IsPrefix of
      true -> [{string:substr(Key, string:len(Prefix) + 1), Val} | Acc];
      _ -> Acc
    end | Res ], Found or IsPrefix );

extract_one_attr([], _, [], Res, Found) -> {lists:reverse(Res), Found}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tr([], _AttrTR, _AttrTD, Acc) -> lists:reverse(Acc);

tr([newline | T], AttrTR, AttrTD, Acc) -> tr(T, AttrTR, AttrTD, Acc);

tr(["|" | T], AttrTR, AttrTD, Acc) ->
  {Tds, Rest} = td(T, AttrTD, [], []),
  tr(Rest, AttrTR, AttrTD, [zml:new_tag(tr, AttrTR, Tds) | Acc]);

tr([H | _], _AttrTR, _AttrTD, _Acc) -> erlang:error(
  "Table line must start with '|', got '" ++ H ++ "' instead.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

td(["|" | T], Attrs, Acc, Tds) ->
  td(T, Attrs, [], [zml:new_tag(td, Attrs, lists:reverse(Acc)) | Tds]);

td([newline | T], _Attrs, [], Tds) -> {lists:reverse(Tds), T};

td([newline | _], _, _, _) -> erlang:error("Table line must end with '|'.");

td([], _Attrs, [], Tds) -> {lists:reverse(Tds), []};

td([], _, _, _) -> erlang:error("Table line must end with '|'.");

td([H | T], Attrs, Acc, Tds) -> td(T, Attrs, [H | Acc], Tds).

