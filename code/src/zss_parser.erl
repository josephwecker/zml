-module(zss_parser).

-export([parse/1, combine_dups/1]).

-define(CSS_LEN_TYPES,
  ["%","em","ex","px","in","cm","mm","pt","pc"]).

-define(E_FLUSH(Tok),
  case script_var(lists:reverse(TAcc)) of
    nothing ->
      tokenize_expression(T, LN, break, [], [Tok | Acc]);
    {str, S} ->
      S2 = inject_variables(S),
      tokenize_expression(T, LN, break, [], [Tok | [{val, S2} | Acc]]);
    Val ->
      tokenize_expression(T, LN, break, [], [Tok | [{val, Val} | Acc]])
  end).

-define(E_FLUSH2,
  case script_var(lists:reverse(TAcc)) of
    nothing ->
      tokenize_expression(T, LN, break, [], Acc);
    {str, S} ->
      S2 = inject_variables(S),
      tokenize_expression(T, LN, break, [], [{val, S2} | Acc]);
    Val ->
      tokenize_expression(T, LN, break, [], [{val, Val} | Acc])
  end).

-define(CVAL(Col), lists:max([lists:min([Col,255]),0])).
-define(AVAL(Alpha), lists:max([lists:min([Alpha,1]),0])).

-define(OP(Op,A,B),
  case Op of
    $+ -> A + B;
    $- -> A - B;
    $/ -> A / B;
    $* -> A * B
  end).

parse([]) ->
  [];
parse(Tokens) ->
  erase(),   %% WARNING!!! Clobbers current process dictionary
  EvaluatedT = evaluate_code(Tokens),
  Clumps = clumper(EvaluatedT),
  {[], {ChildRules, []}} = get_children([{}], Clumps),
  Rules = lists:map(fun format_rules/1, ChildRules),
  combine_dups(Rules).

combine_dups(Rules) ->
  [First | Rest] = lists:sort(Rules),
  combine_dups(Rest, First, []).
combine_dups([],Last,Acc) ->
  lists:reverse([Last | Acc]);
combine_dups([{LastKey,V} | T],{LastKey, VAcc},Acc) ->
  combine_dups(T, {LastKey, lists:sort(V ++ VAcc)}, Acc);
combine_dups([New | T], Last, Acc) ->
  combine_dups(T, New, [Last | Acc]).

format_rules({Selectors, Attributes}) ->
  {lists:sort(lists:map(fun tuple_to_list/1, Selectors)),
    lists:map(
        fun({Key,Val}) ->
            case Val of
              "" -> {Key, "0"};
              _ -> {Key, Val}
            end
        end, Attributes)}.

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

format_clump(code, Codes) ->
  {code, lists:map(
    fun({_,_,A}) ->
      string:sub_string(string:strip(A),2,length(A) - 1)
    end, Codes)};
format_clump(include, Includes) ->
  {include, lists:map(
    fun({_,_,A}) ->
      string:sub_string(string:strip(A),1,length(A) - 1)
    end, Includes)};
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

% TODO: Include files here

%get_children(Parents, [{include, Incs} | T], {RuleAcc, AttAcc}) ->
%  IncCode = lists:map(fun(IncludeFile) -> zss:compile(IncludeFile)),

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

script_var([]) ->        nothing;
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
        {_, Rem} when is_list(Rem) ->
          case lists:member(Rem, ?CSS_LEN_TYPES) of
            true -> {list_to_atom(Rem), N};
            false -> {str, Str}
          end;
        _ ->
          {str, Str}
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


evaluate_code(Toks) ->
  lists:reverse(evaluate_code(Toks, [])).

% TODO: Here's where we need to do includes as well
evaluate_code([], Acc) ->
  Acc;
evaluate_code([{assignment, LN, Text} | T], Acc) ->
  Str = string:sub_string(string:strip(Text), 2, length(Text) - 1),
  {Left, Right} = lists:split(string:chr(Str, $=), Str),
  Key = string:strip(string:sub_string(Left, 1, length(Left) - 1)),
  ValText = string:strip(Right),
  put("V" ++ Key, evaluate_expression(ValText, LN)),
  evaluate_code(T, Acc);
evaluate_code([{attr, LN, Text} | T], Acc) ->
  Text2 = pull_and_evaluate_exprs(Text, LN, []),
  evaluate_code(T, [{attr, LN, Text2} | Acc]);
evaluate_code([H | T], Acc) ->
  evaluate_code(T, [H | Acc]).

pull_and_evaluate_exprs([], _, Acc) ->
  lists:reverse(Acc);
pull_and_evaluate_exprs([$[ | T], LN, Acc) ->
  {T2, Expression} = pull_full_expression(T, LN, 1, []),
  ExpVal = translate_value(evaluate_expression(Expression, LN)),
  pull_and_evaluate_exprs(T2, LN,
    lists:reverse(ExpVal) ++ Acc);
pull_and_evaluate_exprs([H | T], LN, Acc) ->
  pull_and_evaluate_exprs(T, LN, [H | Acc]).

pull_full_expression([], _, _, Acc) ->
  {[], lists:reverse(Acc)};
pull_full_expression([$[ | T], LN, Lvl, Acc) ->
  pull_full_expression(T, LN, Lvl + 1, [$[ | Acc]);
pull_full_expression([$] | T], LN, Lvl, Acc) ->
  Lvl2 = Lvl - 1,
  case Lvl2 of
    0 ->
      {T, lists:reverse(Acc)};
    _ ->
      pull_full_expression(T, LN, Lvl2, [$] | Acc])
  end;
pull_full_expression([H | T], LN, Lvl, Acc) ->
  pull_full_expression(T, LN, Lvl, [H | Acc]).

evaluate_expression(Expr, LN) ->
  Tokens = tokenize_expression(Expr, LN, inword, [], []),
  RPN = shunt_yard(LN, Tokens, [], []),
  process_rpn(LN, RPN, []).

tokenize_expression([], _LN, _, [], Acc) ->
  lists:reverse(Acc);
tokenize_expression([] = T, LN, _, TAcc, Acc) ->
  ?E_FLUSH2;
tokenize_expression([$( | T], LN, _, TAcc, Acc) ->
  ?E_FLUSH(dstart);
tokenize_expression([$) | T], LN, _, TAcc, Acc) ->
  ?E_FLUSH(dend);
tokenize_expression([$  | T], LN, _, TAcc, Acc) ->
  ?E_FLUSH2;
tokenize_expression([$* | T], LN, _, TAcc, Acc) ->
  ?E_FLUSH({operator, {2,$*}});
tokenize_expression([$/ | T], LN, _, TAcc, Acc) ->
  ?E_FLUSH({operator, {2,$/}});
tokenize_expression([$- | T], LN, break, TAcc, Acc) ->
  ?E_FLUSH({operator, {1,$-}});
tokenize_expression([$+ | T], LN, _, TAcc, Acc) ->
  ?E_FLUSH({operator, {1,$+}});
tokenize_expression([H | T], LN, _, TAcc, Acc) ->
  tokenize_expression(T, LN, inword, [H | TAcc], Acc).

shunt_yard(LN, [dstart|T], OpStack, Output) ->
  shunt_yard(LN, T, [dstart|OpStack], Output);
shunt_yard(LN, [{operator, {1,_}}|_] = L, [{2,Op2} | OpStack], Output) ->
  shunt_yard(LN, L, OpStack, [{2,Op2} | Output]);
shunt_yard(LN, [{operator, {2,_}}|_] = L, [{2,Op2} | OpStack], Output) ->
  shunt_yard(LN, L, OpStack, [{2,Op2} | Output]);
shunt_yard(LN, [{operator, Op} | T], OpStack, Output) ->
  shunt_yard(LN, T, [Op | OpStack], Output);
shunt_yard(LN, [{val, Val} | T], OpStack, Output) ->
  shunt_yard(LN, T, OpStack, [Val | Output]);
shunt_yard(LN, [dend | T], [Op | OpStackT], Output) when Op /= dstart ->
  shunt_yard(LN, [dend | T], OpStackT, [Op | Output]);
shunt_yard(LN, [dend | T], [dstart | OpStackT], Output) ->
  shunt_yard(LN, T, OpStackT, Output);
shunt_yard(LN, [dend | _], [], _) ->
  erlang:error("Mismatched parenthases in zss file line " ++
    integer_to_list(LN));
shunt_yard(LN, [], [Op | _], _)
when (Op == dend) or (Op == dstart) ->
  erlang:error("Mismatched parenthases in zss file line " ++
    integer_to_list(LN));
shunt_yard(LN, [], [Op | OpStack], Output) ->
  shunt_yard(LN, [], OpStack, [Op | Output]);
shunt_yard(_LN, [], [], Output) ->
  lists:reverse(Output).


process_rpn(_LN, [], [ResVal]) ->
  ResVal;
process_rpn(LN, [], _) ->
  erlang:error("Cannot understand zss arithmetic expression- too many arguments line " ++ integer_to_list(LN));
process_rpn(LN, [{N,Op} | T], [V2 | [V1 | Res]]) when is_integer(N) ->
  process_rpn(LN, T, [calculate(LN, Op, V1, V2) | Res]);
process_rpn(LN, [V | T], Res) ->
  process_rpn(LN, T, [V | Res]).

% Color and Color
calculate(_LN, O, {col,{R,G,B,A}}, {col, {R2,G2,B2,_A2}}) ->
  {col,{?OP(O,R,R2), ?OP(O,G,G2), ?OP(O,B,B2), A}};

% Color and Number - Alpha is ignored
calculate(_LN, O, {num, N}, {col, {R,G,B,A}}) ->
  {col,{?OP(O,N,R),?OP(O,N,G),?OP(O,N,B),A}};
calculate(_LN, O, {col, {R,G,B,A}}, {num, N}) ->
  {col,{?OP(O,R,N),?OP(O,G,N),?OP(O,B,N),A}};

calculate(LN, Op, {Type, _}, {col, _}) ->
  erlang:error("Cannot '" + [Op] + "' a '" ++ Type ++
    "' and a color together- on line " ++ integer_to_list(LN));
calculate(LN, Op, {col, _}, {Type, _}) ->
  erlang:error("Cannot '" + [Op] + "' a '" ++ Type ++
    "' and a color together- on line " ++ integer_to_list(LN));

% Number to number
calculate(_LN, O, {num, N1}, {num, N2}) ->
  {num, ?OP(O,N1,N2)};

% Number to size
calculate(_LN, O, {num, N1}, {Type, N2}) when is_integer(N2) or is_float(N2) ->
  {Type, ?OP(O,N1,N2)};
calculate(_LN, O, {Type, N1}, {num, N2}) when is_integer(N1) or is_float(N1) ->
  {Type, ?OP(O,N1,N2)};

% Size to size - simple case where they match first
calculate(_LN, O, {Type, N1}, {Type, N2}) when is_integer(N1) or is_float(N1) ->
  {Type, ?OP(O,N1,N2)}.

%calculate(_LN, O, {T1, N1}, {T2, N2}) when
%    (is_integer(N1) or is_float(N1)) and
%    (is_integer(N2) or is_float(N2)) ->





translate_value({col, {R, G, B, 1}}) ->
  condense_color(lists:flatten(io_lib:format("#~2.16.0b~2.16.0b~2.16.0b",
        [?CVAL(round(R)),?CVAL(round(G)),?CVAL(round(B))])));
translate_value({col, {R, G, B, A}}) ->
  lists:flatten(io_lib:format("rgba(~p,~p,~p,~p)",
      [?CVAL(round(R)), ?CVAL(round(G)), ?CVAL(round(B)), ?AVAL(A)]));
translate_value(L) when is_list(L) ->
  L;
translate_value({num, N}) when is_integer(N) ->
  io_lib:format("~p",[N]);
translate_value({num, N}) when is_float(N) ->
  io_lib:format("~f",[N]);

translate_value({Type,N}) when is_integer(N) ->
  lists:flatten(io_lib:format("~p~s",[N,atom_to_list(Type)]));
translate_value({Type,N}) when is_float(N) ->
  lists:flatten(io_lib:format("~f~s",[N,atom_to_list(Type)])).

condense_color([$#,R1,R2,G1,G2,B1,B2]) when
(R1 == R2) and (G1 == G2) and (B1 == B2) ->
  [$#,R1,G1,B1];
condense_color(V) ->
  V.

% TODO:
%  Differentiate here between strings and variables based on something else.
inject_variables(S) ->
  case get("V" ++ S) of
    undefined ->
      {str, S};
    SavedVal ->
      SavedVal
  end.
