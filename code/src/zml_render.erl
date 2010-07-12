
-module(zml_render).

-export([render/1, render/2]).

render(Template) -> render(Template, [], fake).

render(Template, Data) -> render(Template, [], Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

render([], Acc, _Data) -> lists:reverse(Acc);

render([[Ch|_] = Str | T], Acc, Data) when is_integer(Ch) ->
  render(T, [Str | Acc], Data);

render([{var, Var} | T], Acc, Data) ->
  render(T, [var(Data, Var) | Acc], Data);

render([{with, [Name], Children} | T], Acc, Data) ->
  Block = lists:flatmap(
    fun(Rec) -> render(Children, [], Rec) end, data(Data, Name)),
  render(T, [Block | Acc], Data);

render([H | T], Acc, Data) when is_list(H) ->
  render(T, [render(H, [], Data) | Acc], Data);

render([H | T], Acc, Data) ->
  render(T, [H | Acc], Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

var(fake, Var) -> "[VARIABLE:" ++ string:join(Var, ".") ++ "]";

var(Props, [Var]) ->
  proplists:get_value(Var, Props, var(fake, [Var]));

var(Props, [H|T]) -> var(proplists:get_value(H, Props, []), T).


data(fake, _Name) -> [fake];
data(Props, Name) -> var(Props, Name).

