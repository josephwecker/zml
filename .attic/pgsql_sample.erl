
-module(pgsql_sample).
-compile(export_all).

merchants() ->
  {ok,[{column,<<"idmerchant">>,int4,4,-1,0},
       {column,<<"merchantname">>,varchar,-1,104,0},
       {column,<<"logourl">>,varchar,-1,504,0},
       {column,<<"lastupdated">>,date,4,-1,0},
       {column,<<"isdeleted">>,bit,-1,1,0}],
      [{<<"100059">>,<<"B.A. Mason">>,
        <<"http://img.shopzilla.com/merchant/100059.gif">>,
        <<"2009-01-10">>,<<"0">>},
       {<<"100134">>,<<"LeGrand Treasures">>,
        <<"http://img.shopzilla.com/merchant/100134.gif">>,
        <<"2009-01-21">>,<<"0">>},
       {<<"100293">>,<<"Austad\\'s Golf">>,
        <<"http://img.shopzilla.com/merchant/100293.gif">>,
        <<"2009-01-21">>,<<"0">>},
       {<<"100436">>,<<"Need Plumbing Supplies">>,
        <<"http://img.shopzilla.com/merchant/100436.gif">>,
        <<"2009-01-21">>,<<"0">>}]}.

cols(Cols) ->
  [{binary_to_list(Name), {Pos, Rec}} ||
   {Pos, {column, Name, _Type, _Prec, _Len, _} = Rec}
     <- lists:zip(lists:seq(1, length(Cols)), Cols) ].

get_col(Name, Row, Cols) -> get_col(Name, Row, Cols, undefined).

get_col(Name, Row, Cols, Default) ->
  case proplists:get_value(Name, Cols) of
    {Pos, _Col} -> element(Pos, Row);
    undefined -> Default
  end.

