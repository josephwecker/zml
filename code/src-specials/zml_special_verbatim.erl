
-module(zml_special_verbatim).

-export([is_recursive/0, tokenize/3]).


is_recursive() -> {non_recursive, no_attrs, no_class_attrs}.

tokenize("verbatim", _Attr, Acc) ->
  zml_util:intersperse(lists:reverse(Acc), "\n").

