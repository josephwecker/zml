
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
    new_tag/3,
    new_tag/4,
    get_tag/2,
    replace_tag/3,
    update_tag/5,
    append_children/3,
    intersperse/2,
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

new_tag(Name, Attr, Children) when is_atom(Name) ->
  new_tag(atom_to_list(Name), normal, Attr, Children);
new_tag(Name, Attr, Children) when is_tuple(Name) ->
  new_tag(Name, special, Attr, Children).

new_tag(Name, Type, Attr, Children) when is_atom(Name) ->
  new_tag(atom_to_list(Name), Type, Attr, Children);
new_tag(Name, Type, Attr, Children) ->
  {Name, Type, Attr, Children}.

%% Recursively replaces any tags with existing Name with the new attributes and
%% children specified. Search is the search path as a list of tag names, such as:
%% [{"html", ID}, "body", "table"] to find the table tag within the body tag
%% within the special html tag with the given ID.  No mechanism yet for
%% wildcard IDs, wildcard tagnames, looking at attributes, or "lookahead" at
%% children.
replace_tag(AST, Search, NewTag) ->
  replace_tag(AST, lists:reverse(Search), NewTag, [], []).

replace_tag([], _, _, _, Acc) ->
  lists:reverse(Acc);

replace_tag([{Name,Tp,Att,Children} | T], Search, NewTag, CurrPath, Acc) ->
  EqPath = lists:sublist([Name | CurrPath], length(Search)),
  case EqPath == Search of
    true ->
      % We won't recurse into new children, because that would inevitably lead
      % to infinite recursion if someone tried to take advantage of it.
      replace_tag(T, Search, NewTag, CurrPath, [NewTag | Acc]);
    false ->
      NewChildren = replace_tag(Children,Search,NewTag, [Name | CurrPath],[]),
      replace_tag(T, Search, NewTag, CurrPath,
        [{Name,Tp,Att,NewChildren} | Acc])
  end;

replace_tag([H|T], Search, NewTag, CurrPath, Acc) ->
  replace_tag(T, Search, NewTag, CurrPath, [H | Acc]).

%% Shortcut for basically changing the attributes / children of a specific tag.
update_tag(AST, [F | _] = Search, Type, NewAttr, NewChildren) when
    is_list(Search) and (is_list(F) or is_tuple(F)) ->
  replace_tag(AST, Search, new_tag(lists:last(Search), Type, NewAttr, NewChildren));
update_tag(AST, Name, Type, NewAttr, NewChildren) ->
  replace_tag(AST, [Name], new_tag(Name, Type, NewAttr, NewChildren)).

append_children(AST, Search, NewChildren) ->
  {_,Type,Attr,Children} = get_tag(AST, Search),
  update_tag(AST, Search, Type, Attr, Children ++ NewChildren).

%% Similar to replace_tag, only is stops when it finds it and returns it
%% instead of returning a rebuilt full AST.
get_tag(AST, Search) ->
  get_tag(AST, lists:reverse(Search), []).

get_tag([], _, _) -> undefined;

get_tag([{Name,_,_,Children} = Tag | T], Search, CurrPath) ->
  EqPath = lists:sublist([Name | CurrPath], length(Search)),
  case EqPath == Search of
    true ->
      Tag;
    false ->
      case get_tag(Children, Search, [Name | CurrPath]) of
        undefined ->
          get_tag(T, Search, CurrPath);
        FoundTag ->
          FoundTag
      end
  end;

get_tag([_|T], Search, CurrPath) -> get_tag(T, Search, CurrPath).


str(A) when is_atom(A) -> atom_to_list(A);
str(A) -> A.

intersperse([H|T], Sep)      -> intersperse(T, Sep, [H]).
intersperse([H|T], Sep, Acc) -> intersperse(T, Sep, [H, Sep | Acc]);
intersperse([],   _Sep, Acc) -> lists:reverse(Acc).


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

