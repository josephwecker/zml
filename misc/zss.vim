" Vim syntax file
" Language: ZSS
" Maintainter: Joseph Wecker
" Latest Revision: 2010-02-05
"
" ZSS mode for gvim, part of ZML, from scratch except some lines stolen from
" css.vim, muahaha
"
" Public Domain
"

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

syn case ignore

syn keyword zssPreDefColors black silver gray white maroon red fuchsia green lime olive yellow navy blue teal aqua contained

syn keyword cssTagName abbr acronym address applet area a b base
syn keyword cssTagName basefont bdo big blockquote body br button
syn keyword cssTagName caption center cite code col colgroup dd del
syn keyword cssTagName dfn dir div dl dt em fieldset font form frame
syn keyword cssTagName frameset h1 h2 h3 h4 h5 h6 head hr html img i
syn keyword cssTagName iframe img input ins isindex kbd label legend li
syn keyword cssTagName link map menu meta noframes noscript ol optgroup
syn keyword cssTagName option p param pre q s samp script select small
syn keyword cssTagName span strike strong style sub sup tbody td
syn keyword cssTagName textarea tfoot th thead title tr tt ul u var
syn keyword cssTagName table object

syn match   zssSpecial        "&"
syn region  zssMixin          start="\["ms=s+1 end="\]"me=e-1
syn region  zssCode           start="\[.*="ms=s+1 end="\]"me=e-1 contains=zssInnerBrackets,zssAssignmentKey,zssPropertyVal

syn match   zssAssignmentKey  ".\{-}="me=e-1 contained

syn match   zssValNumbers     "[^A-Za-z]\@<=[0-9.]*" contained
syn match   zssValTypes       "\(px\|em\|pt\|%\)" contained
syn keyword zssValBools true false contained
syn region  zssPropertyVal    start="="hs=s+1 end="]"me=e-1 contained contains=zssStrInnerBrackets,zssValNumbers,zssValTypes,zssValBools,zssPreDefColors,zssColors
syn region  zssColors         start="[rRHh][gGSs][bBLl][aA]\{0,1}(" end=")" contained contains=zssValNumbers,zssValTypes
syn match   zssColors         "#\([0-9A-Fa-f]\{6}\|[0-9A-Fa-f]\{3}\)" contained

syn region  zssInnerBrackets  start="\[" end="\]" contained
syn region  zssStrInnerBrackets start="\[" end="\]" contained

" Comments
syn match   zssComment	      "||.*$" contains=zmlTodo,@Spell
syn region  zssMLComment      start="|#" end="#|" contains=zmlTodo,@Spell
syn keyword zssToDo           TODO FIXME XXX contained
syn region  zssMLString       start="|\"" end="\"|"

syn match   zssProperty       "\s\@<=:[A-Za-z0-9_-]\+.*$"hs=s+1 contains=zssPropertyValActual,zssStartProp
syn region  zssValCode        start="\[" end="\]" contained contains=zssValNumbers,zssValTypes,zssValBools,zssPreDefColors,zssColors
syn match   zssPropertyValActual "\s.*$" contained contains=zssValCode,zssValNumbers,zssValTypes,zssValBools,zssPreDefColors,zssColors
"syn region  zssValCode        start="\[" end="\(]\|$\)" contained contains=zssInnerBrackets

syn match   zssIdTag          "\(\s\+\|^\)#[^ \t]*" contains=zssTrickyAttrId,zssTrickyAttrClass
syn match   zssClassTag       "\(\s\+\|^\)\.[^ \t]*" contains=zssTrickyAttrId,zssTrickyAttrClass
syn match   zssTrickyAttrId   "#[^\. \t]\+"
syn match   zssTrickyAttrClass "\.[^# \t]\+"

syn match   zssStartProp      ":" contained

if version >= 508 || !exists("did_zss_syn_inits")
  if version <= 508
    let did_zss_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink zssCode                Special
  HiLink zssInnerBrackets	Special
  HiLink zssStrInnerBrackets	String
  HiLink zssAssignmentKey       Function

  HiLink cssTagName             Keyword
  HiLink zssComment		Comment
  HiLink zssMLComment           SpecialComment
  HiLink zssMLString            String

  HiLink zssProperty            Operator

  HiLink zssIdTag               Number
  HiLink zssTrickyAttrId        Number
  HiLink zssClassTag            Float
  HiLink zssTrickyAttrClass     Float

  HiLink zssPropertyVal         String
  HiLink zssPropertyValActual   String
  HiLink zssValBools            Boolean
  HiLink zssValTypes            Type
  HiLink zssValNumbers          Number
  HiLink zssPreDefColors        Identifier
  HiLink zssColors              Identifier

  HiLink zssMixin               Label
  HiLink zssSpecial             Delimiter
  HiLink zssStartProp           Delimiter
  delcommand HiLink
endif

let b:current_syntax = "zss"
