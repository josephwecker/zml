%% Special handler for *table tags in zml
%%
%% Author: Sergei Matusevich <sergei.matusevich@gmail.com>
%%

-module(zml_special_table).

-export([process_node/2]).


process_node({{"table", ID}, special, Attr, Children}, _Options) ->
  {[AttrTR, AttrTD], AttrTable} = extract_attrs(["tr_", "td_"], Attr),
  TableRows = tr(Children, AttrTR, AttrTD, [[]]),
  {{"table", ID}, special, AttrTable, TableRows}.

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

tr([], _AttrTR, _AttrTD, [AccL | AccR]) ->
  lists:reverse([lists:reverse(AccL) | AccR]);

tr([newline | T], AttrTR, AttrTD, Acc) -> tr(T, AttrTR, AttrTD, Acc);
tr([[]      | T], AttrTR, AttrTD, Acc) -> tr(T, AttrTR, AttrTD, Acc);

tr([[$\\, $| | W] | T], AttrTR, AttrTD, [AccL | AccR]) ->
  tr([W | T], AttrTR, AttrTD, [[$| | AccL] | AccR]);

tr([[$| | W] | T], AttrTR, AttrTD, [AccL | AccR]) ->
  {Tds, Rest} = td([W | T], AttrTD, [[]], []),
  tr(Rest, AttrTR, AttrTD,
    [[], zml_util:new_tag(tr, AttrTR, Tds), lists:reverse(AccL) | AccR]);

tr([[_ | W] | T], AttrTR, AttrTD, Acc) -> tr([W | T], AttrTR, AttrTD, Acc);

tr([W | T], AttrTR, AttrTD, Acc) when is_tuple(W) -> tr(T, AttrTR, AttrTD, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

td([[$| | W] | T], Attrs, [AccL | AccR], Tds) ->
  td([W | T], Attrs, [[]], [zml_util:new_tag(td, Attrs,
    lists:reverse([lists:reverse(AccL) | AccR])) | Tds]);

td([[$\\, $| | W] | T], Attrs, [AccL | AccR], Tds) ->
  td([W | T], Attrs, [[$| | AccL] | AccR], Tds);

td([[] | T], Attrs, Acc, Tds) -> td(T, Attrs, Acc, Tds);

td([newline | T], _Attrs, _Acc, Tds) -> {lists:reverse(Tds), T };
td([],            _Attrs, _Acc, Tds) -> {lists:reverse(Tds), []};

td([[Ch | W] | T], Attrs, [AccL | AccR], Tds) ->
  td([W | T], Attrs, [[Ch | AccL] | AccR], Tds);

td([W | T], Attrs, [AccL | AccR], Tds) when is_tuple(W) ->
  td(T, Attrs, [[], W, lists:reverse(AccL) | AccR], Tds).

