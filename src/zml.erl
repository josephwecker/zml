-module(zml).

-compile(export_all).


compile(InFile) ->
  AST = zml_hand_parser:parse(zml_tokenizer:tokenize_file(InFile)),
  AST.
  %translate_ast_item(AST, []).

translate_ast_item([], Acc) ->
  lists:reverse(Acc);
translate_ast_item([String | [Next | _] = T], Acc)
    when is_list(String) and is_list(Next) ->
  translate_ast_item(T, [list_to_binary(String) | [<<" ">> | Acc]]);
translate_ast_item([String | T], Acc) when is_list(String) ->
  translate_ast_item(T, [list_to_binary(String) | Acc]);
translate_ast_item([_Tag | T], Acc) ->
  translate_ast_item(T, [<<"(some tag)">> | Acc]).
