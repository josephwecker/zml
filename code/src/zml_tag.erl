
-module(zml_tag).
-compile(export_all).
-include("zml_tokenizer.hrl").

-define(AST_NEWLINE, "\n").

inline_tags(Lines, Attr, Level) ->
  {Body, Rest} = inline_tags(Lines, Level, [], []),
  {Attr, Body, Rest}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inline_tags([[$|, $| | _] | T], Level, AccL, AccR) ->
  inline_tags(T, Level, [], add_text(AccL, AccR));

inline_tags([[$|, Q | Ln] | T], Level, AccL, AccR) when ?IS_QUOTE(Q) ->
  {L,R} = parse_quote([Ln | T], Q, AccL),
  inline_tags(R, Level, L, AccR);

inline_tags([[$\\, Ch | Ln] | T], Level, AccL, AccR)
    when ?IS_TAG(Ch) orelse Ch == $| orelse Ch == $; ->
  inline_tags([Ln | T], Level, [Ch | AccL], AccR);

inline_tags([[$; | Ln] | T], Level, AccL, AccR) when Level > 0 ->
  {lists:reverse(add_text(AccL, AccR)), [Ln | T]};

inline_tags([[$$, Ch | Ln] | T], Level, AccL, AccR) when ?IS_BR_OPEN(Ch) ->
  case parse_var(Ln, Ch) of
    fail  -> inline_tags([Ln | T], Level, [Ch, $$ | AccL], AccR);
    {L,R} -> inline_tags([R | T], Level, [], [L | add_text(AccL, AccR)])
  end;

inline_tags([[Ch | W] | T] = Lines, Level, AccL, AccR) when ?IS_TAG(Ch) ->
  case zml_indent:get_tokenizer(Lines) of
    {_,no_tokenizer,_} -> inline_tags([W | T], Level, [Ch | AccL], AccR);
    {recursive, {Type, Tag, Attr}, Rest} ->
      {NewAttr, Body, NewRest} = inline_tags(Rest, Attr, Level + 1),
      {NewTag, []} = inline_special(Type, Tag, NewAttr, Body),
      inline_tags(NewRest, Level, [], [NewTag | add_text(AccL, AccR)]);
    {non_recursive, {Type, Tag, Attr}, Rest} ->
      {NewTag, NewRest} = inline_special(Type, Tag, Attr, Rest),
      inline_tags(NewRest, Level, [], [NewTag | add_text(AccL, AccR)])
  end;

inline_tags([[] | T], 0, AccL, AccR) ->
  inline_tags(T, 0, [], [newline | add_text(AccL, AccR)]);

inline_tags([[] | T], Level, AccL, AccR) ->
  inline_tags(T, Level, [], add_text(AccL, AccR));

inline_tags([Tag | T], Level, AccL, AccR) when is_tuple(Tag) ->
  inline_tags(T, Level, [], [Tag | add_text(AccL, AccR)]);

inline_tags([[Ch | Ln] | T], Level, AccL, AccR) ->
  inline_tags([Ln | T], Level, [Ch | AccL], AccR);

inline_tags([], _Level, AccL, AccR) ->
  {lists:reverse(add_text(AccL, AccR)), []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inline_special(tag, {special, Id}, Attr, Body) ->
  zml_util:call_special(Id, inline_tag,
    [Id, Attr, Body], {{{Id, 0}, special, Attr, Body}, []});

inline_special(tag, Id, Attr, Body) -> {{Id, normal, Attr, Body}, []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_attr([[Ch | W] | T], Attr) when ?IS_BR_OPEN(Ch) ->
  parse_attr([W | T], none, [], [], {Ch, close_bracket(Ch), 0}, Attr);

parse_attr([[Ch | W] | T], Attr) when ?IS_WHITESPACE(Ch) ->
  parse_attr([W | T], Attr);

parse_attr(Lines, Attr) -> {Attr, Lines}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_attr([[] | T], Id, AccL, AccR, Br, Attr) ->
  parse_attr(T, Id, [], add_text(AccL, AccR), Br, Attr);

parse_attr([[$|, $| | _] | T], Id, AccL, AccR, Br, Attr) ->
  parse_attr([[] | T], Id, AccL, AccR, Br, Attr);

parse_attr([[$|, Ch | W] | T], Id, AccL, AccR, Br, Attr) when ?IS_QUOTE(Ch) ->
  {L,R} = parse_quote([W | T], Ch, AccL),
  parse_attr(R, Id, L, AccR, Br, Attr);

parse_attr([[$$, Ch | W] | T], Id, AccL, AccR, Br, Attr) when ?IS_BR_OPEN(Ch) ->
  case parse_var(W, Ch) of
    fail  -> parse_attr([W | T], Id, [Ch, $$ | AccL], AccR, Br, Attr);
    {L,R} -> parse_attr([R | T], Id, [], [L | add_text(AccL, AccR)], Br, Attr)
  end;

parse_attr([[Open | W] | T], Id, AccL, AccR, {Open, Close, L}, Attr) ->
  parse_attr([W | T], Id, [Open | AccL], AccR, {Open, Close, L+1}, Attr);

parse_attr([[Close | W] | T], Id, AccL, AccR, {_, Close, 0}, Attr) ->
  {add_attr(Id, AccL, AccR, Attr), [W | T]};

parse_attr([[Close | W] | T], Id, AccL, AccR, {Open, Close, L}, Attr) ->
  parse_attr([W | T], Id, [Close | AccL], AccR, {Open, Close, L-1}, Attr);

parse_attr([[$\\, Close | W] | T], Id, AccL, AccR, {_,Close,_} = Br, Attr) ->
  parse_attr([W | T], Id, [Close | AccL], AccR, Br, Attr);

parse_attr([[$\\, Ch | W] | T], Id, AccL, AccR, Br, Attr)
    when ?IS_WHITESPACE(Ch) orelse Ch == $| orelse Ch == $$ ->
  parse_attr([W | T], Id, [Ch | AccL], AccR, Br, Attr);

parse_attr([W | T], Id, AccL, AccR, Br, Attr) ->
  case zml_indent:parse_id_spc(W) of
    {[_|_] = NewId, [$:, Ch | R]} when ?IS_WHITESPACE(Ch) ->
      parse_attr([R | T], string:strip(NewId, left),
        [], [], Br, add_attr(Id, AccL, AccR, Attr));
    {[_|_] = NewId, ":"} ->
      parse_attr(T, string:strip(NewId, left),
        [], [], Br, add_attr(Id, AccL, AccR, Attr));
    {[_|_] = NewId, R} ->
      parse_attr([R | T], Id, append_rev(NewId, AccL), AccR, Br, Attr);
    {[], [Ch | R]} ->
      parse_attr([R | T], Id, [Ch | AccL], AccR, Br, Attr)
  end;

parse_attr([], Id, AccL, AccR, _Br, Attr) ->
  {add_attr(Id, AccL, AccR, Attr), []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_quote([[Q, $| | W] | T], Q, Acc) -> {Acc, [W | T]};
parse_quote([[Ch | W] | T], Q, Acc) -> parse_quote([W | T], Q, [Ch | Acc]);
parse_quote([[] | T], Q, Acc) -> parse_quote(T, Q, [?AST_NEWLINE | Acc]);
parse_quote([], _, Acc) -> {Acc, []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_var(Ln, Br) -> parse_var([$. | Ln], close_bracket(Br), []).

parse_var([Br | T], Br, Acc) -> {{var, lists:reverse(Acc)}, T};

parse_var([$. | T], Br, Acc) ->
  case zml_indent:parse_id(T) of
    {[], _} -> fail;
    {Id, Rest} -> parse_var(Rest, Br, [Id | Acc])
  end;

parse_var(_Ln, _Br, _Acc) -> fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_text([],   AccR) -> AccR;
add_text(AccL, AccR) -> [lists:reverse(AccL) | AccR].


add_attr(none, _AccL, _AccR, Attr) -> Attr;

add_attr(Id, AccL, AccR, Attr) ->
 zml_util:append_attr(Attr, {Id, lists:reverse(add_text(AccL, AccR))}).


append_rev([H|T], Acc) -> append_rev(T, [H|Acc]);
append_rev([],    Acc) -> Acc.


close_bracket($() -> $);
close_bracket($[) -> $];
close_bracket(${) -> $}.

