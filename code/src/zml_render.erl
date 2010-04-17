
-module(zml_render).

-export([render/1, render/2]).

% TODO: think of a better interface
-define(RND_VAR_NAMES, fake).

% TODO: implement fake storage that returns names of variables
% and data sets instead of the actual data?
render(Template) -> render(Template, [], ?RND_VAR_NAMES).

render(Template, Data) -> render(Template, [], Data).


render([], Acc, _Data) -> lists:reverse(Acc);

render([[Ch|_] = Str | T], Acc, Data) when is_integer(Ch) ->
  render(T, [Str | Acc], Data);

render([{var, Var} | T], Acc, Data) ->
  render(T, [var(Var, Data) | Acc], Data);

render([{with, _Sets, Children} | T], Acc, Data) ->
  % FIXME: no looping over data for now!
  Block = render(Children, [], Data),
  render(T, [Block | Acc], Data);

render([H | T], Acc, Data) when is_list(H) ->
  render(T, [render(H, [], Data) | Acc], Data);

render([H | T], Acc, Data) -> render(T, [H | Acc], Data).


var(Var, fake) -> [$$ | Var].

