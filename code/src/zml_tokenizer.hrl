
-define(FLUSH(Other),
  case CurrTAcc of
    [] -> [Other | AllTAcc];
    _ -> [[Other, {string, get(line_num), lists:reverse(CurrTAcc)}] | AllTAcc]
  end).
-define(SFLUSH,
  case CurrTAcc of
    [] -> AllTAcc;
    _ -> [{string, get(line_num), lists:reverse(CurrTAcc)} | AllTAcc]
  end).

-define(IS_WHITESPACE(H),
  (H >= $\x{0009} andalso H =< $\x{000D})
    orelse H == $\x{0020} orelse H == $\x{00A0}).

-define(IS_ATTR(H), H == $# orelse H == $.).
-define(IS_TAG(H),  H == $: orelse H == $* orelse ?IS_ATTR(H)).

-define(IS_QUOTE(Ch),    Ch == $" orelse Ch == $' orelse Ch == $`).
-define(IS_BR_OPEN(Ch) , Ch == $( orelse Ch == $[ orelse Ch == ${).
-define(IS_BR_CLOSE(Ch), Ch == $) orelse Ch == $] orelse Ch == $}).

-define(T_ATTR_ST, $().
-define(T_ATTR_EN, $)).
-define(T_ESC, $\\).

-define(T_IGN_INL_1, $|).
-define(T_IGN_INL_2, $|).

-define(T_IGN_MLT_ST_1, $|).
-define(T_IGN_MLT_ST_2, $#).
-define(T_IGN_MLT_EN_1, $#).
-define(T_IGN_MLT_EN_2, $|).

-define(T_STR_MLT_ST_1, $|).
-define(T_STR_MLT_ST_2, 34). % dbl-quote
-define(T_STR_MLT_EN_1, 34).
-define(T_STR_MLT_EN_2, $|).

-define(T_TAG_ST, $:).
-define(T_TAG_SPECIAL_ST, $*).
-define(T_TAG_CLASS_ST, $.).
-define(T_TAG_ID_ST, $#).
-define(T_INL_TAG_D, $;).

-define(T_ATTR_DELIM, $:).

-define(T_CODE_ST, $[).
-define(T_CODE_EN, $]).

