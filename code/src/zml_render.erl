
-module(zml_render).

-export([render/1, render/2]).

render(Template) -> render(Template, [], fun data_fake/2).

render(Template, Data) -> render(Template, [], data_accessor(Data)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

render([], Acc, _GetData) -> lists:reverse(Acc);

render([[Ch|_] = Str | T], Acc, GetData) when is_integer(Ch) ->
  render(T, [Str | Acc], GetData);

render([{var, Vars} | T], Acc, GetData) ->
  render(T, [GetData(var, Vars) | Acc], GetData);

render([{with, Vars, Children} | T], Acc, GetData) ->
  Block = render_with(Children, [], Vars, GetData(with, Vars)),
  render(T, [Block | Acc], GetData);

render([H | T], Acc, GetData) when is_list(H) ->
  render(T, [render(H, [], GetData) | Acc], GetData);

render([H | T], Acc, GetData) -> render(T, [H | Acc], GetData).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

render_with(_AST, Acc, _Vars, eod) -> lists:reverse(Acc);

render_with(AST, Acc, Vars, GetData) ->
  render_with(AST, [render(AST, [], GetData) | Acc],
              Vars, GetData(next, Vars)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_accessor(fake) -> fun data_fake/2;

data_accessor(
    {[{column, _Name, _Type, _Size, _Modifier, _Format} | _] = Cols, Rows}) ->
  fun(Op, Vars) -> data_pgsql(Op, Vars, Rows, pgsql_cols(Cols)) end;

data_accessor({ok, Cols, Rows}) -> data_accessor({Cols, Rows});

data_accessor([{[Ch|_], _} | _] = Props) when is_integer(Ch) ->
  fun(Op, Vars) -> data_proplist(Op, [Props], Vars) end;

data_accessor(Props) ->
  fun(Op, Vars) -> data_proplist(Op, Props, Vars) end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_fake(var, [Ch|_] = Var)
  when is_integer(Ch) -> "[VARIABLE:" ++ Var ++ "]";

data_fake(var, Vars) -> "[VARIABLE:" ++ string:join(Vars, ".") ++ "]";

data_fake(with, _Vars) -> fun data_fake/2;

data_fake(next, _Vars) -> eod.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_proplist(var, [Props|_], [Var]) ->
  proplists:get_value(Var, Props, data_fake(var, [Var]));

data_proplist(var, [Props|_], [H|T]) ->
  data_proplist(var, proplists:get_value(H, Props, [[]]), T);

data_proplist(var, [], Vars) -> data_fake(var, Vars);

data_proplist(with, [],  _Vars) -> eod;
data_proplist(next, [],  _Vars) -> eod;
data_proplist(next, [_], _Vars) -> eod;

data_proplist(with, Props, Vars) ->
  data_accessor(data_proplist(var, Props, Vars));

data_proplist(next, [_|T], _Vars) ->
  fun(Op, Vars) -> data_proplist(Op, T, Vars) end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pgsql_cols(Cols) ->
  [{binary_to_list(Name), {Pos, Rec}} ||
   {Pos, {column, Name, _Type, _Size, _Modifier, _Format} = Rec}
     <- lists:zip(lists:seq(1, length(Cols)), Cols) ].

data_pgsql(var, Vars, [], _Cols) -> data_fake(var, Vars);

data_pgsql(var, [Name], [Row|_], Cols) ->
  case proplists:get_value(Name, Cols) of
    {Pos, _Col} -> element(Pos, Row);
    undefined -> data_fake(var, [Name])
  end;

data_pgsql(with, _Vars, [],  _Cols) -> eod;
data_pgsql(next, _Vars, [],  _Cols) -> eod;
data_pgsql(next, _Vars, [_], _Cols) -> eod;

data_pgsql(with, Vars, Rows, Cols) ->
  data_accessor(data_pgsql(var, Vars, Rows, Cols));

data_pgsql(next, _Vars, [_|Rows], Cols) ->
  fun(Op, Vars) -> data_pgsql(Op, Vars, Rows, Cols) end.

