" Vim syntax file
" Language: Zippy Markup Language
" Maintainer: Joseph Wecker
" Latest Revision: 2009-09-17
"
" ZML mode for (g)vim, unfortunately from scratch.
" 

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif
syn match   zmlComment	"#.*$" display contains=zmlTodo,@Spell
syn keyword zmlTodo	TODO FIXME XXX contained
" Strings
syn region zmlString	start=+'+ skip=+\\\\\|\\'\|\\$+ excludenl end=+'+ end=+$+ keepend contains=zmlEscape,zmlEscapeError,@Spell
syn region zmlString	start=+"+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end=+$+ keepend contains=zmlEscape,zmlEscapeError,@Spell
syn region zmlString	start=+"""+ end=+"""+ keepend contains=zmlEscape,zmlEscapeError,zmlDocTest2,zmlSpaceError,@Spell
syn region zmlString	start=+'''+ end=+'''+ keepend contains=zmlEscape,zmlEscapeError,zmlDocTest,zmlSpaceError,@Spell

syn region zmlDocstring  start=+^\s*[uU]\?[rR]\?"""+ end=+"""+ keepend excludenl contains=zmlEscape,@Spell,zmlDoctest,zmlDocTest2,zmlSpaceError
syn region zmlDocstring  start=+^\s*[uU]\?[rR]\?'''+ end=+'''+ keepend excludenl contains=zmlEscape,@Spell,zmlDoctest,zmlDocTest2,zmlSpaceError

syn match  zmlEscape		+\\[abfnrtv'"\\]+ display contained
syn match  zmlEscape		"\\\o\o\=\o\=" display contained
syn match  zmlEscapeError	"\\\o\{,2}[89]" display contained
syn match  zmlEscape		"\\x\x\{2}" display contained
syn match  zmlEscapeError	"\\x\x\=\X" display contained
syn match  zmlEscape		"\\$"


syn match zmlGenericTag		"^\s*:\w*"
syn match zmlInlineTag		":\w*:"
syn match zmlIdTag              "^\s*%\w*"
syn match zmlClassTag           "^\s*\.\w*"
syn match zmlSpecialTag         "^\s*\*\w*"

syn match zmlIndentError	"^\s*\( \t\|\t \)\s*\S"me=e-1 display
"syn match zmlSpaceError	        "\s\+$" display

syn region zmlTagParams start="("ms=s+1 end=")"me=e-1 transparent contains=zmlString,zmlStraightParam,zmlMoreParams
syn match zmlStraightParam "[a-zA-Z0-9_-]\+\s*=\s*[a-zA-Z_-]\+" contained contains=zmlParam,zmlParamVal
syn match zmlParam "[a-zA-Z_-]\+\s*="me=e-1 contained
syn match zmlParamVal "=\s*[a-zA-Z0-9_-]\+"ms=s+1 contained
syn match zmlMoreParams "[a-zA-Z_-]\+\s*="me=e-1 contained contains=zmlStraightParam
"syn region  zmlFuncParams start="("ms=s+1 end=")"me=e-1 transparent contains=zmlParam 
"syn region  zmlParam start="[a-zA-Z_]" end="\(,\|)\s*:\)" contained contains=zmlString,zmlParamName,zmlParamDefault,zmlDefaultAssignment transparent nextgroup=zmlParam
"syn match zmlParamName "[a-zA-Z_][a-zA-Z0-9_]*" contained nextgroup=zmlDefaultAssignment skipwhite skipnl
"syn match zmlDefaultAssignment "=" nextgroup=zmlParamDefault skipwhite contained skipnl

"syn match zmlParamDefault "=\@<=[^,]*" contained transparent contains=@zmlStringType,@zmlNumberType,@zmlBuiltin,zmlKeyword

if version >= 508 || !exists("did_zml_syn_inits")
  if version <= 508
    let did_zml_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink zmlString		String
  HiLink zmlDocString		String
  HiLink zmlComment		Comment
  HiLink zmlEscape		Special
  HiLink zmlEscapeError		Error
  HiLink zmlIndentError		Error
  HiLink zmlSpaceError		Error

  HiLink zmlTodo		Todo

  HiLink zmlGenericTag		Entity
  HiLink zmlInlineTag		Entity
  HiLink zmlIdTag		Entity
  HiLink zmlClassTag		Entity
  HiLink zmlSpecialTag		Conditional

  HiLink zmlParam		Test
  HiLink zmlMoreParams		Test
  HiLink zmlParamVal		String


  delcommand HiLink
endif

let b:current_syntax = "zml"
