-module(zss_parser).

-export([parse/1]).

-define(F3(A,B,C),
  [A | [B | [C | T]]]).

parse([]) ->
  [];
parse(Tokens) ->
  Clumps = clumper(Tokens),
  parse(Clumps, []).

clumper([]) ->
  [];
clumper([{Type,_,_} = FirstTok | T]) ->
  clumper(T, Type, [FirstTok], []).
% Finished
clumper([], _, [], Acc) ->
  lists:reverse(Acc);
clumper([], _, [{Type,_,_} | _] = CAcc, Acc) ->
  lists:reverse([{Type, CAcc} | Acc]);
% Same type
clumper([{Type,_,_} = Tok | T], LastType, CAcc, Acc) when
    (Type == LastType) and (Type /= dedent) ->
  clumper(T, Type, [Tok | CAcc], Acc);
% Different type
clumper([{Type,_,_} = Tok | T], LastType, CAcc, Acc) ->
  clumper(T, Type, [Tok], [{LastType, CAcc} | Acc]).


parse([], Acc) ->
  lists:flatten(lists:reverse(Acc));
parse(Tokens, Acc) ->
  {Rules, Toks} = get_rule([], Tokens, []),
  case Rules of
    [] ->
      parse(Toks, Acc);
    _ ->
      parse(Toks, [Rules | Acc])
  end.

get_rule(_Parents, [{dedent, _} | T], RAcc) ->
  {lists:reverse(RAcc), T};
get_rule(Parents, [{sel, Sels} | T], RAcc) ->
  %io:format("~p | ~p | ~p | ~p ~n", [Parents, Sels, T, RAcc]),
  {LineAttrs, T2} = get_line_attrs(T),
  {Indented, ChildAttrs, T3} = get_child_attrs(T2),
  ActualSels = list_mult(lists:map(fun({_,_,A})->list_to_tuple(A) end, Sels), Parents),
  RAcc2 =
    case LineAttrs ++ ChildAttrs of
      [] ->
        RAcc;
      Atts ->
        FSels = lists:map(fun tuple_to_list/1, lists:sort(ActualSels)),
        [{FSels, lists:sort(att_list(Atts))} | RAcc]
    end,
  case {T3, Indented} of
    {_,true} ->
      get_rule(ActualSels, T3, RAcc2);
    {[{indent,_}|T4], false} ->
      get_rule(ActualSels, T4, RAcc2);
    _ ->
      {RAcc2, T3}
  end;
get_rule(_,L,RAcc) ->
  {lists:reverse(RAcc), []}.

get_line_attrs([{attr, Atts} | T]) ->
  {lists:map(fun({_,_,A})->A end, Atts), T};
get_line_attrs(T) ->
  {[],T}.

get_child_attrs([{indent,_} | [{attr, Atts} | T]]) ->
  {true, lists:map(fun({_,_,A})->A end, Atts), T};
get_child_attrs(T) ->
  {false, [],T}.

list_mult(L1,[]) ->
  L1;
list_mult(L1,L2) ->
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
          end, L1)
    end, L2)).

att_list(AttStrs) ->
  lists:map(
    fun(Str) ->
        case string:chr(Str, $ ) of
          0 -> {Str, "0"};
          P ->
            {Key, Val} = lists:split(P, Str),
            {string:strip(Key), Val}
        end
    end, AttStrs).
