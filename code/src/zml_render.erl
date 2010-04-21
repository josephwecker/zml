
-module(zml_render).

-export([render/1, render/2]).

render(Template) -> render(Template, [], fake, []).

render(Template, Data) -> render(Template, [], Data, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

render([], Acc, _Data, _Context) -> lists:reverse(Acc);

render([[Ch|_] = Str | T], Acc, Data, Context) when is_integer(Ch) ->
  render(T, [Str | Acc], Data, Context);

render([{var, Var} | T], Acc, Data, Context) ->
  render(T, [var(Data, [Var | Context]) | Acc], Data, Context);

render([{with, [Name], Children} | T], Acc, Data, Context) ->
  Ctx = [Name | Context],
  Block = lists:flatmap(fun(Rec) ->
      render(Children, [], Rec, Ctx)
    end, data(Data, Ctx)),
  render(T, [Block | Acc], Data, Context);

render([H | T], Acc, Data, Context) when is_list(H) ->
  render(T, [render(H, [], Data, Context) | Acc], Data, Context);

render([H | T], Acc, Data, Context) ->
  render(T, [H | Acc], Data, Context).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

var(fake, [H|T]) ->
  [$$ | lists:foldl(fun(X,A) -> X ++ [$.|A] end, H, T)];

var(Props, [H]) -> proplists:get_value(H, Props);
var(Props, [H|T]) -> var(proplists:get_value(H, Props, []), T).


data(fake, _Ctx) -> [fake];

data(Props, Ctx) -> var(Props, Ctx).

