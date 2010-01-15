-module(zml).

-compile(export_all).


compile(InFile) ->
  AST = zml_hand_parser:parse(zml_tokenizer:tokenize_file(InFile)),
  translate_ast_item(AST, []).

translate_ast_item([], Acc) ->
  lists:reverse(Acc);
translate_ast_item([String | [Next | _] = T], Acc)
    when is_list(String) and is_list(Next) ->
  translate_ast_item(T, [" " | [String | Acc]]);
translate_ast_item([String | T], Acc) when is_list(String) ->
  translate_ast_item(T, [String | Acc]);
translate_ast_item([{Code,code,[],Children} | T], Acc) ->
  ToAppend = ["!!CODE!!",
    string:join(Code, " "),
    "!!",
    translate_ast_item(Children, []),
    "!!END!!"],
  translate_ast_item(T, [ToAppend | Acc]);
translate_ast_item([{Name,Type,Attributes,[]} | T], Acc) ->
  ToAppend = ["<", Name,
    translate_attributes(Attributes), "/>"],
  translate_ast_item(T, [ToAppend | Acc]);
translate_ast_item([{Name,Type,Attributes,Children} | T], Acc) ->
  ToAppend = [
    "<", Name,
    translate_attributes(Attributes), ">",
    translate_ast_item(Children, []),
    "</", Name, ">"],
  translate_ast_item(T, [ToAppend | Acc]).

translate_attributes([]) ->
  "";
translate_attributes(Atts) when is_tuple(Atts) ->
  lists:foldl(fun out_attr/2, [], dict:to_list(Atts));
translate_attributes(Atts) ->
  lists:foldl(fun out_attr/2, [], Atts).
out_attr({Name, Values}, Acc) ->
  [" ", Name, "=\"", string:join(Values, " "), "\"" | Acc].
