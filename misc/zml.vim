" Vim syntax file
" Language: ZML
" Maintainer: Joseph Wecker
" Latest Revision: 2010-01-13
"
" ZML mode for (g)vim, unfortunately from scratch.
"
" Public Domain
" 

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" Comments
syn match   zmlComment	 "||.*$" display contains=zmlTodo,@Spell
syn region  zmlMLComment start="|#" end="#|" display contains=zmlTodo,@Spell
syn keyword zmlToDo      TODO FIXME XXX contained

syn region zmlMLString   start="|\"" end="\"|"

syn match zmlGenericTag		"\(\s\+\|^\):[^ \t(;]*" contains=zmlTrickyAttrId,zmlTrickyAttrClass
syn match zmlIdTag              "\(\s\+\|^\)#[^ \t(;]*" contains=zmlTrickyAttrId,zmlTrickyAttrClass
syn match zmlClassTag           "\(\s\+\|^\)\.[^ \t(;]*" contains=zmlTrickyAttrId,zmlTrickyAttrClass
syn match zmlSpecialTag         "\(\s\+\|^\)\*[^ \t(;]*" contains=zmlTrickyAttrId,zmlTrickyAttrClass

syn match zmlAttrName           "[^ \t]\+[^\\]\:" contained contains=zmlComment
syn region zmlAttributes start="("ms=s+1 end=")"me=e-1 contains=zmlMLString,zmlMLComment,zmlAttrName,zmlComment

syn match zmlTrickyAttrId       "#[^\. \t;]\+" contained
syn match zmlTrickyAttrClass    "\.[^# \t;]\+" contained

if version >= 508 || !exists("did_zml_syn_inits")
  if version <= 508
    let did_zml_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink zmlComment		Comment
  HiLink zmlMLComment           Comment
  HiLink zmlMLString            String

  HiLink zmlGenericTag          Function
  HiLink zmlIdTag               Number
  HiLink zmlTrickyAttrId        Number
  HiLink zmlClassTag            Float
  HiLink zmlTrickyAttrClass     Float
  HiLink zmlSpecialTag          Statement

  HiLink zmlAttributes          String
  HiLink zmlAttrName            Identifier

  delcommand HiLink
endif

let b:current_syntax = "zml"

