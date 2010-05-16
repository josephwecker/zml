
-module(zml_special_comment).

-export([is_recursive/0, tokenize/3]).


is_recursive() -> non_recursive.

tokenize("comment", _Attr, _Acc) -> [].

