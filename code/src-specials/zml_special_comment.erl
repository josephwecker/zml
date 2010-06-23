
-module(zml_special_comment).

-export([is_recursive/0, tokenize/3, inline_tag/3]).


is_recursive() -> {non_recursive, no_attrs, no_class_attrs}.

tokenize("comment", _Attr, _Acc) -> [].

% inline_tag("comment", _Attr, [_|T]) -> {[], T };

inline_tag("comment", _Attr, _Body) -> {[], []}.

