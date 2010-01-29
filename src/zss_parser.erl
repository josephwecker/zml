-module(zss_parser2).

-export([parse/1]).

parse([]) ->
  [];
parse(Tokens) ->
  Clumps = clumper(Tokens),
  {[], {ChildRules, []}} = get_children([{}], Clumps),
  lists:sort(lists:map(fun format_rules/1, ChildRules)).

format_rules({Selectors, Attributes}) ->
  {lists:sort(lists:map(fun tuple_to_list/1, Selectors)),
    lists:sort(Attributes)}.

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
            0 -> {A, "0"};
            P ->
              {Key, Val} = lists:split(P, A),
              {string:strip(Key), Val}
          end
      end, Attrs)}.

% Returns {RemainingClumps, {ChildRules, ChildAttributes}}
get_children(Parents, Clumps) ->
  %io:format("{~p}",[?LINE]),
  get_children(Parents, Clumps, {[],[]}).

get_children(_Parents, [{end_of_file,_} | T], {RuleAcc, AttAcc}) ->
  %io:format("{~p}",[?LINE]),
  {T, {lists:sort(lists:flatten(RuleAcc)),
      lists:sort(lists:flatten(AttAcc))}};

get_children(_Parents, [{dedent,_} | T], {RuleAcc, AttAcc}) ->
  %io:format("{~p}",[?LINE]),
  {T, {lists:sort(lists:flatten(RuleAcc)),
      lists:sort(lists:flatten(AttAcc))}};

get_children(Parents, [{attr,Atts} | T], {RuleAcc, AttAcc}) ->
  %io:format("{~p}",[?LINE]),
  {T2, NewAtts} = get_attributes(Atts, T),
  get_children(Parents, T2, {RuleAcc, [NewAtts | AttAcc]});

%get_children(Parents, [{sel,Sels} | [{att, Attr} | [{indent,_} | T]]],
%    {RuleAcc, AttAcc}) ->
  % Build combined selectors
  % Get children rules & attributes
  % Combine children attributes with Attr
  % Combine Attributes with combined sels for current rules
  % Update RuleAcc with children rules and current rules - no update to AttAcc

get_children(Parents, [{sel,Sels} | [{indent,_} | T]], {RuleAcc, AttAcc}) ->
  %io:format("{~p}",[?LINE]),
  ActualSels = multiply_selectors(Parents, Sels),
  {T2, {Rules, Atts}} = get_children(ActualSels, T),
  get_children(Parents, T2,
    {[Rules | [new_rules(ActualSels, Atts) | RuleAcc]], AttAcc}).

%get_children(Parents, [{sel,Sels} | [{att, Attr} | T]], {RuleAcc, AttAcc}) ->
  % Build combined selectors
  % Combine Attributes with selectors for current rules
  % Update RuleAcc, ignore AttAcc

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
  %io:format("{~p}",[?LINE]),
  % TODO: combine atts and children of last Att
  {T, ChildAtts};
get_attributes(_Atts, [{indent,_} | _]) ->
  %io:format("{~p}",[?LINE]),
  erlang:error("Only attributes can be children of attributes");
get_attributes(Atts, T) ->
  %io:format("{~p}",[?LINE]),
  {T, Atts}.
