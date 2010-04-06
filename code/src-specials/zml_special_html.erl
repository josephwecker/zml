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
    fun process_head_and_body/5,
    fun process_xhtml/5,
    fun process_metas/5,
%    fun process_javascript/5,
    fun process_zss_and_images/5,
    fun process_cleanup/5
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
  [Type] = zml:get_attr_vals(type, Attr, ?DEFAULT_TYPE),
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
  ExistingHead = zml:get_tag(Children, ["head"]),
  ExistingBody = zml:get_tag(Children, ["body"]),

  {Head, AllButHead} =
    case ExistingHead of
      undefined ->
        {zml:new_tag("head", normal, [], []), Children};
      _ ->
        {ExistingHead, zml:replace_tag(Children, ["head"], [])}
    end,
  Body =
    case ExistingBody of
      undefined ->
        zml:new_tag(body, normal, [], AllButHead);
      _ ->
        ExistingBody
    end,
  zml:update_tag(AST, {"html",ID}, special, Attr, [Head, Body]).

process_xhtml(ID, Attr, Children, AST, _Options) ->
  [[TypeFC | _]] = zml:get_attr_vals(type, Attr, ?DEFAULT_TYPE),
  case TypeFC == $x of
    false ->
      AST;
    true ->
      [Namespace] = zml:get_attr_vals(xmlns, Attr, ?XMLNS),
      [Language] = zml:get_attr_vals("xml:lang",Attr, ?LANGUAGE_XML_DEFAULT),
      zml:update_tag(AST, {"html",ID}, special,
        [{"xmlns",[Namespace]}, {"xml:lang",[Language]} | Attr], Children)
      %% Skipping for now - xml declaration
      % TODO: flag to force insertion of the xml declaration
      %Encoding = get_html_attr(encoding, Attr, ?ENCODING_DEFAULT),
      %[?ENC_TOP_X(Encoding) | AST]
  end.

process_metas(ID, Attr, _Children, AST, _Options) ->
  [[Tp | _]] = zml:get_attr_vals(type, Attr, ?DEFAULT_TYPE),

  Metas = lists:foldr(fun(Input,Acc) -> new_metas(Input, Acc, Attr) end, [],
    [
      {encoding,    Tp, ?ENCODING_DEFAULT},
      {language,    Tp, ?LANGUAGE_DEFAULT},
      {description, Tp, none},
      {keywords,    Tp, none},
      {copyright,   Tp, none},
      {nosmarttag,  Tp, true},
      {title,       Tp, none},
      {favicon,     Tp, none}]),
  zml:append_children(AST, [{"html",ID},"head"], Metas).

process_zss_and_images(ID, Attr, Children, AST, Options) ->
  % TODO:
  %   - Use source_filename's path + search_path + erlang search path for
  %     finding externals (build into zml?)
  %   - Figure out list of zss files
  %   - Pull in zss ASTs
  %   - Cull AST based on ZML AST
  %   - (Future) build list of images from culled zss and do image processing
  %   - Render css inline and attach to AST
  zs_html_zss_images:process(ID, Attr, Children, AST, Options).

process_cleanup(ID, Attr, Children, AST, _Options) ->
  CleanAttrs = lists:foldl(fun proplists:delete/2, Attr, ?SPECIAL_ATTRIBUTES),
  zml:update_tag(AST, {"html",ID}, special, CleanAttrs, Children).


new_metas({Name, Type, Def}, Acc, Attr) ->
  case zml:get_attr_vals(Name, Attr, Def) of
    ["none"] -> Acc;
    Vals ->
      metatag(Name, Type, Vals) ++ Acc
  end.

metatag(encoding, IsXml, [Val]) ->
  build_meta("http-equiv", "Content-Type",
             ["text/html;", "charset=" ++ to_upper(Val)], IsXml);

metatag(language, IsXml, Vals) ->
  build_meta("http-equiv", "Content-Language", Vals, IsXml);

metatag(copyright, IsXml, Vals) ->
  build_meta(name, copyright, ["Copyright (c)" | Vals], IsXml, false);

metatag(nosmarttag, IsXml, _) ->
  build_meta(name, "MSSmartTagsPreventParsing", ["TRUE"], IsXml);

metatag(title, _, Vals) -> [zml:new_tag(title, [], Vals)];

metatag(favicon, _, Vals) ->
  [zml:new_tag(link, [{"rel", ["icon"]}, {"href", Vals}], []),
   zml:new_tag(link, [{"rel", ["shortcut icon"]}, {"href", Vals}], [])];

metatag(Name, IsXml, Vals) -> build_meta(name, Name, Vals, IsXml).

build_meta(Key, Name, Vals, IsXml) ->
  build_meta(Key, Name, Vals, IsXml, IsXml).

build_meta(Key, Name, Vals, IsXml, LowerVals) ->
  {NewName, End} = case {Name, IsXml} of
    {"MSSmartTagsPreventParsing", $x} -> {Name, "/>"};
    {_, $x} -> {to_lower(zml:str(Name)), "/>"};
    {_, _ } -> {zml:str(Name), ">" }
  end,
  NewVals = case LowerVals of
    $x -> to_lower(join(Vals, " "));
    _  -> join(Vals, " ")
  end,
  ["<meta " ++ zml:str(Key) ++ "=\"" ++ NewName ++
    "\" content=\"" ++ NewVals ++ "\"" ++ End].

