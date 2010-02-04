-module(zss_parser).

-export([parse/1]).

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
      {string:strip(string:sub_string(Left, 1, length(Left) - 1)), string:strip(Right)}
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
  % TODO!! Turn this into a lfold to accumulate Rules / Attributes, and finish the function
  lists:foreach(fun(Code) ->
    {T2, {Rules, Atts}} =
      case get("M" ++ Code) of
        undefined ->
          erlang:error("[" ++ Code ++ "] not defined before being used!"),
          {[], {[], []}};
        Vals ->
          get_children(Parents, Vals, {RuleAcc, AttAcc})
      end
    end, Codes)

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
