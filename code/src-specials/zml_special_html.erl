%% Special handler for *html tags in zml
%%
%% Author: Joseph Wecker <joseph.wecker@gmail.com>
%%

-module(zml_special_html).

-export([run_handler/5]).

-include("zml_special_html.hrl").
-import(string, [to_lower/1, to_upper/1, join/2]).

% Each processor is run in order and returns an updated AST.  The fold function
% updates Attr and Children as per the new AST between each call so that if,
% for example, a sub-function removes some attributes from the special handler,
% it will be reflected in both the AST and the ID/Attr/Children on the next
% call.
run_handler(ID, _Attr, _Children, AST, Options) ->
  Transformations = [
    fun process_doctype/5,
    fun process_head_and_body/5
%    fun process_javascript/5,
%    fun process_xhtml/5,
%    fun process_zss_and_images/5,
%    fun process_metas/5,
%    fun process_special_attributes/5
  ],
  lists:foldl(
    fun(Transformer, NewAST) ->
      {_, _, NewAttr, NewChildren} = zml:get_tag(NewAST, [{"html",ID}]),
      Transformer(ID, NewAttr, NewChildren, NewAST, Options)
    end, AST, Transformations).

process_doctype(_ID, Attr, _Children, AST, _Options) ->
  [FirstLine | _] = AST,
  case is_list(FirstLine) of
    true ->
      case (string:sub_word(FirstLine,1) == "<!DOCTYPE") of
        true -> erlang:error("Please do not declare a DOCTYPE- use a " ++ 
            "'type' attribute on the *html tag instead.");
        false -> ok
      end;
    _ -> ok
  end,
  Type = zml:get_attr(type, Attr, ?DEFAULT_TYPE),
  DoctypeString =
    case proplists:get_value(list_to_atom(Type), ?TYPES) of
      undefined ->
        Allowed = string:join(lists:map(
            fun atom_to_list/1,
            proplists:get_keys(?TYPES)),", "),
        erlang:error("'" ++ Type ++
          "' html type unknown. Try one of: " ++ Allowed);
      DS -> DS
    end,
  [DoctypeString | AST].

process_head_and_body(ID, Attr, Children, AST, _Options) ->
  % Ensure there is a body
  {AST2, Children2} =
    case zml:get_tag(Children, ["body"]) of
      undefined ->
        % Temporarily remove head if any, add rest to a "body" which
        % then becomes the new children of the html tag
        AllButHead = zml:replace_tag(Children, ["head"], []),
        NewChildren = [zml:new_tag(body, normal, dict:new(), AllButHead)],
        {zml:update_tag(AST, {"html",ID}, special, Attr, NewChildren),
          NewChildren};
      _ ->
        {AST, Children}
    end,
  % Ensure there is a head
  % Look in original children for that head
  case zml:get_tag(Children, ["head"]) of
    undefined ->
      % Pop in a blank head
      Children3 = [zml:new_tag("head", normal, dict:new(), []) | Children2],
      zml:update_tag(AST2, {"html",ID}, special, Attr, Children3);
    ExistingHead ->
      Children3 = [ExistingHead | Children2],
      zml:update_tag(AST2, {"html",ID}, special, Attr, Children3)
  end.
