Nonterminals tags tag attributes strings.
Terminals
	indent dedent newline
	string
	start_tag start_attrs finish_attrs inline_delim.

Rootsymbol tags.
Endsymbol end_of_file.

strings -> string string : [value_of($1), value_of('$2')].
strings -> strings string : '$1' ++ [value_of('$2')].

attributes -> start_attrs string finish_attrs : value_of('$2').
attributes -> start_attrs strings finish_attrs : '$2'.

tags -> tag tag : ['$1', '$2'].
tags -> tags tag : '$1' ++ ['$2'].

tag -> start_tag string attributes newline : {value_of('$2'), '$3', []}.

Erlang code.
value_of(Token) ->
    element(3, Token).

