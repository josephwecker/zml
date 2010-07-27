
-define(IS_WHITESPACE(H),
  (H >= $\x{0009} andalso H =< $\x{000D})
    orelse H == $\x{0020} orelse H == $\x{00A0}).

-define(IS_ALPHA(Ch),
  (Ch >= $a andalso Ch =< $z) orelse (Ch >= $A andalso Ch =< $Z)).

-define(IS_ATTR(H), H == $# orelse H == $.).
-define(IS_TAG(H),  H == $: orelse H == $* orelse ?IS_ATTR(H)).

-define(IS_QUOTE(Ch),    Ch == $" orelse Ch == $' orelse Ch == $`).
-define(IS_BR_OPEN(Ch) , Ch == $( orelse Ch == $[ orelse Ch == ${).
-define(IS_BR_CLOSE(Ch), Ch == $) orelse Ch == $] orelse Ch == $}).

