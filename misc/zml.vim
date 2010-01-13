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

syn match zmlGenericTag		"\(\s\+\|^\):\w*"
syn match zmlIdTag              "\(\s\+\|^\)#\w*"
syn match zmlClassTag           "\(\s\+\|^\)\.\w*"
syn match zmlSpecialTag         "\(\s\+\|^\)\*\w*"

syn match zmlAttrName           "[^ \t]\+\:" contained
syn region zmlAttributes start="("ms=s+1 end=")"me=e-1 contains=zmlMLString,zmlMLComment,zmlAttrName,zmlComment

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

  HiLink zmlGenericTag          Type
  HiLink zmlIdTag               Type
  HiLink zmlClassTag            Type
  HiLink zmlSpecialTag          Statement

  HiLink zmlAttributes          String
  HiLink zmlAttrName            Identifier

  delcommand HiLink
endif

let b:current_syntax = "zml"

