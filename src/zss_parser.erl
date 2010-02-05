-module(zss_parser).

-export([parse/1, script_var/1]).

parse([]) ->
  [];
parse(Tokens) ->
  Clumps = clumper(Tokens),
  %io:format("~p", [Clumps]),
  erase(),   %% WARNING!!! Clobbers current process dictionary
  {[], {ChildRules, []}} = get_children([{}], Clumps),
  lists:sort(lists:map(fun format_rules/1, ChildRules)).

format_rules({Selectors, Attributes}) ->
  {lists:sort(lists:map(fun tuple_to_list/1, Selectors)),
    lists:sort(lists:map(
        fun({Key,Val}) ->
            case Val of
              "" -> {Key, "0"};
              _ -> {Key, Val}
            end
        end, Attributes))}.

clumper([]) ->
  [];
clumper([{Type,_,_} = FirstTok | T]) ->
  clumper(T, Type, [FirstTok], []).
% Finished
clumper([], _, [], Acc) ->
  lists:reverse(Acc);
clumper([], _, [{Type,_,_} | _] = CAcc, Acc) ->
  lists:reverse([format_clump(Type, CAcc) | Acc]);
% Same type
clumper([{Type,_,_} = Tok | T], LastType, CAcc, Acc) when
    (Type == LastType) and (Type /= dedent) ->
  clumper(T, Type, [Tok | CAcc], Acc);
% Different type
clumper([{Type,_,_} = Tok | T], LastType, CAcc, Acc) ->
  clumper(T, Type, [Tok], [format_clump(LastType, CAcc) | Acc]).


format_clump(assignment, Asmts) ->
  {assignment, lists:map(
    fun({_,_,A}) ->
      Str = string:sub_string(string:strip(A), 2, length(A) - 1),  % Remove brackets
      {Left,Right} = lists:split(string:chr(Str,$=), Str),
      io:format("~p~n",[script_var(string:strip(Right))]),
      {string:strip(string:sub_string(Left, 1, length(Left) - 1)),
        script_var(string:strip(Right))}
    end, Asmts)};
format_clump(code, Codes) ->
  {code, lists:map(
    fun({_,_,A}) ->
      string:sub_string(string:strip(A),2,length(A) - 1)
    end, Codes)};
format_clump(indent, _) ->
  {indent, none};
format_clump(end_of_file, _) ->
  {end_of_file, none};
format_clump(dedent, _) ->
  {dedent, none};
format_clump(sel, Sels) ->
  {sel, lists:map(fun({sel,_,A}) -> list_to_tuple(A) end, Sels)};
format_clump(attr, Attrs) ->
  {attr,lists:map(
      fun({attr,_,A}) ->
          case string:chr(A, $ ) of
            0 -> {A, ""};
            P ->
              {Key, Val} = lists:split(P, A),
              {string:strip(Key), Val}
          end
      end, Attrs)}.

% Returns {RemainingClumps, {ChildRules, ChildAttributes}}
get_children(Parents, Clumps) ->
  get_children(Parents, Clumps, {[],[]}).

get_children(_Parents, [{end_of_file,_} | T], {RuleAcc, AttAcc}) ->
  {T, {lists:sort(lists:flatten(RuleAcc)),
      lists:sort(lists:flatten(AttAcc))}};

get_children(_Parents, [{dedent,_} | T], {RuleAcc, AttAcc}) ->
  {T, {lists:sort(lists:flatten(RuleAcc)),
      lists:sort(lists:flatten(AttAcc))}};

get_children(Parents, [{assignment, Asts} | T], {RuleAcc, AttAcc}) ->
  lists:foreach(fun({Key,Val}) -> put("V" ++ Key, Val) end, Asts),
  get_children(Parents, T, {RuleAcc, AttAcc});

get_children(Parents, [{code,Codes} | [{indent,_} | T]], {RuleAcc, AttAcc}) ->
  {T2, RawChildren} = pull_raw_children(T),
  lists:foreach(fun(Key) -> put("M" ++ Key, RawChildren) end, Codes),
  get_children(Parents, T2, {RuleAcc, AttAcc});

get_children(Parents, [{code, Codes} | T], {RuleAcc, AttAcc}) ->
  {RuleAccFin, AttAccFin} = lists:foldl(
      fun(Code, {RA, AA}) ->
        {[], {RA2, AA2}} =
          case get("M" ++ Code) of
            undefined ->
              erlang:error("[" ++ Code ++ "] not defined before being used!"),
              {[], {[], []}};
            Vals ->
              get_children(Parents, Vals, {RA, AA})
          end,
        {RA2, AA2}
      end, {RuleAcc, AttAcc}, Codes),
  get_children(Parents, T, {RuleAccFin, AttAccFin});

get_children(Parents, [{attr,Atts} | T], {RuleAcc, AttAcc}) ->
  {T2, NewAtts} = get_attributes(Atts, T),
  get_children(Parents, T2, {RuleAcc, [NewAtts | AttAcc]});

get_children(Parents, [{sel,Sels} | [{attr, Attr} | [{indent,_} | T]]],
    {RuleAcc, AttAcc}) ->
  ActualSels = multiply_selectors(Parents, Sels),
  {T2, {Rules, Atts}} = get_children(ActualSels, T),
  get_children(Parents, T2,
    {[Rules | [new_rules(ActualSels, Atts ++ Attr) | RuleAcc]], AttAcc});

get_children(Parents, [{sel,Sels} | [{indent,_} | T]], {RuleAcc, AttAcc}) ->
  ActualSels = multiply_selectors(Parents, Sels),
  {T2, {Rules, Atts}} = get_children(ActualSels, T),
  get_children(Parents, T2,
    {[Rules | [new_rules(ActualSels, Atts) | RuleAcc]], AttAcc});

get_children(Parents, [{sel,Sels} | [{attr, Attr} | T]], {RuleAcc, AttAcc}) ->
  ActualSels = multiply_selectors(Parents, Sels),
  get_children(Parents, T, {[new_rules(ActualSels, Attr) | RuleAcc], AttAcc}).

multiply_selectors([{}], Sel2) ->
  Sel2;
multiply_selectors(Sel1, Sel2) ->
  lists:flatten(lists:map(
    fun(A) ->
        lists:map(
          fun(B) ->
              case tuple_to_list(B) of
                [$& | Sel] ->
                  list_to_tuple(tuple_to_list(A) ++ Sel);
                LB ->
                  list_to_tuple(tuple_to_list(A) ++ " " ++ LB)
              end
          end, Sel2)
    end, Sel1)).

pull_raw_children(L) ->
  pull_raw_children(L, [], 1).

pull_raw_children([{dedent,Info} | T], Acc, ILvl) ->
  ILvl2 = ILvl - 1,
  case ILvl2 of
    0 ->
      {T, lists:reverse([{dedent, Info} | Acc])};
    _ ->
      pull_raw_children(T, [{dedent,Info} | Acc], ILvl2)
  end;
pull_raw_children([{indent,Info} | T], Acc, ILvl) ->
  ILvl2 = ILvl + 1,
  pull_raw_children(T, [{indent, Info} | Acc], ILvl2);
pull_raw_children([{end_of_file,_} | T], Acc, _ILvl) ->
  {T, lists:reverse(Acc)};
pull_raw_children([], Acc, _ILvl) ->
  {[], lists:reverse(Acc)};
pull_raw_children([H | T], Acc, ILvl) ->
  pull_raw_children(T, [H | Acc], ILvl).

new_rules(_, []) ->
  [];
new_rules([], _) ->
  [];
new_rules(Sels, Atts) ->
  {lists:sort(Sels), lists:sort(Atts)}.


% Only one level of attribute nesting allowed (:font\n\n:family, for example)
get_attributes(Atts, [{indent,_} | [{attr,ChildAtts} | [{dedent,_} | T]]]) ->
  [{Last, Val} | Rest] = Atts,
  case Val of
    "" -> {T, combine_atts(Last, ChildAtts) ++ Rest};
    _ -> {T, combine_atts(Last, ChildAtts) ++ Rest ++ [{Last, Val}]}
  end;
get_attributes(_Atts, [{indent,_} | _]) ->
  erlang:error("Only attributes can be children of attributes");
get_attributes(Atts, T) ->
  {T, Atts}.

combine_atts(ParName, ChildAtts) ->
  lists:map(
    fun({OldKey, Val}) ->
        {ParName ++ "-" ++ OldKey, Val}
    end, ChildAtts).


script_var("true") ->    {bool, true};
script_var("false") ->   {bool, false};
script_var("black") ->   {col, {0  ,  0,  0,1}};
script_var("silver") ->  {col, {192,192,192,1}};
script_var("gray") ->    {col, {128,128,128,1}};
script_var("white") ->   {col, {255,255,255,1}};
script_var("maroon") ->  {col, {128,  0,  0,1}};
script_var("red") ->     {col, {255,  0,  0,1}};
script_var("fuchsia") -> {col, {255,  0,255,1}};
script_var("green") ->   {col, {  0,128,  0,1}};
script_var("lime") ->    {col, {  0,255,  0,1}};
script_var("olive") ->   {col, {128,128,  0,1}};
script_var("yellow") ->  {col, {255,255,  0,1}};
script_var("navy") ->    {col, {  0,  0,128,1}};
script_var("blue") ->    {col, {  0,  0,255,1}};
script_var("teal") ->    {col, {  0,128,128,1}};
script_var("aqua") ->    {col, {  0,255,255,1}};
script_var([D | _] = Str) when (D >= $0) and (D =< $9) or (D =:= $.) ->
  case float_or_int(Str) of
    {N,[]} ->   {num, N};
    {N,Str2} when is_list(Str2) ->
      case {N, string:strip(string:to_lower(Str2))} of
        {_,"px"} -> {px, N};
        {_,"%"} ->  {per, N};
        {_,"em"} -> {em, N};
        {_,"pt"} -> {pt, N};
        _ ->        {str1, Str}
      end;
    _ ->
      {str, Str}
  end;
script_var("#" ++ Str) ->
  case is_color(string:strip(string:to_lower(Str))) of
    false ->
      {str, "#" ++ Str};
    {R,G,B} ->
      {col, {R,G,B,1}}
  end;
script_var("rgb(" ++ _Rest) ->
  {nyi, nyi};
script_var("rgba(" ++ _Rest) ->
  {nyi, nyi};
script_var("hsl(" ++ _Rest) ->
  {nyi, nyi};
script_var("hsla(" ++ _Rest) ->
  {nyi, nyi};

script_var(Str) ->
  {str, Str}.


% Chunk of helpers for script_var

float_or_int(Str) ->
  case string:to_float(Str) of
    {error, no_float} ->
      case string:to_integer(Str) of
        {error, _} ->
          string:to_float("0" ++ Str);
        Something ->
          Something
      end;
    Other ->
      Other
  end.
is_color(Str) ->
  case length(Str) of
    3 ->
      case is_hex(Str) of
        true ->
          [R,G,B] = Str,
          {hs2d([R]), hs2d([G]), hs2d([B])};
        false ->
          false
      end;
    6 ->
      case is_hex(Str) of
        true ->
          [R1,R2,G1,G2,B1,B2] = Str,
          {hs2d([R1,R2]), hs2d([G1,G2]), hs2d([B1,B2])};
        false ->
          false
      end;
    _ ->
      false
  end.
is_hex(Str) ->
  lists:all(
    fun(A) ->
        (((A >= $0) and (A =< $9)) or ((A >= $a) and (A =< $f)))
    end, Str).
hs2d([N1,N2]) ->
  h2d(N1) * 16 + h2d(N2);
hs2d([N]) ->
  h2d(N) * 16 + h2d(N).
h2d($a) -> 10;
h2d($b) -> 11;
h2d($c) -> 12;
h2d($d) -> 13;
h2d($e) -> 14;
h2d($f) -> 15;
h2d(N) ->
  {I,[]} = string:to_integer([N]),
  I.
