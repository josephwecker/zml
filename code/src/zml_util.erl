
-module(zml_util).

% Utilities for special handlers:
-export([
    append_attr/2,
    prepend_attr/2,
    get_attr_vals/2,
    get_attr_vals/3,
    get_attr_vals_split/2,
    get_attr_vals_split/3,
    split_attr_values/1,
    pop_attr/3,
    str/1,
    call_special/3,
    call_special/4
  ]).


append_attr([{K1,V1} | Attributes], {K1,V2}) ->
  [{K1, V1 ++ V2} | Attributes];
append_attr([KV1 | Attributes], KV2) ->
  [KV1 | append_attr(Attributes, KV2)];
append_attr([], KV2) ->
  [KV2].

prepend_attr([{K1,V1} | Attributes], {K1,V2}) ->
  [{K1, V2 ++ V1} | Attributes];
prepend_attr([KV1 | Attributes], KV2) ->
  [KV1 | prepend_attr(Attributes, KV2)];
prepend_attr([], KV2) -> [KV2].

get_attr_vals(Find, Attr) ->
  proplists:get_value(str(Find), Attr, []).
get_attr_vals(Find, Attr, [H|_] = Default) when is_integer(H) ->
  proplists:get_value(str(Find), Attr, [Default]);
get_attr_vals(Find, Attr, Default) when is_list(Default) ->
  proplists:get_value(str(Find), Attr, [str(V) || V <- Default]);
get_attr_vals(Find, Attr, Default) ->
  proplists:get_value(str(Find), Attr, [str(Default)]).

get_attr_vals_split(Find, Attr) ->
  get_attr_vals_split(Find, Attr, []).
get_attr_vals_split(Find, Attr, Default) ->
  split_attr_values(get_attr_vals(Find, Attr, Default)).

split_attr_values(Attrs) ->
  lists:flatmap(fun(A) -> string:tokens(A, " \t\n") end, Attrs).

pop_attr(Find, Attr, Default) ->
  Key = str(Find),
  Val = proplists:get_value(Key, Attr, [str(Default)]),
  {proplists:delete(Key, Attr), Val}.

str(A) when is_atom(A) -> atom_to_list(A);
str(A) -> A.

call_special(Tag, Func, Args) ->
  call_special(Tag, Func, Args, function_not_found).

call_special(Tag, Func, Args, Default) ->
  ModuleName = list_to_atom("zml_special_" ++ string:to_lower(Tag)),
  case code:ensure_loaded(ModuleName) of
    {module, Module} ->
      case erlang:function_exported(Module, Func, length(Args)) of
        true -> apply(Module, Func, Args);
        _    -> Default
      end;
    {error, _Err} -> Default
  end.

