" Vim syntax file
" Language:    Ear
" Maintainer:  Devyn Cairns <devyn.cairns@gmail.com>
" Version:     1.0
" Last Change: 2010 Nov 14
" URL:         http://devyn.github.com/Ear

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'ear'
endif

syn match   earNumber   "0x[0-9A-Fa-f]\+\|0[0-7]\+\|0b[0-1]\+\|0z[0-9A-Za-z]\+\|[0-9]\+"
syn match   earVariable "\(^|\s\)\@=[A-Z][A-Za-z0-9']*"
syn match   earOperator "[-`+%&*/@$^,?.;:>=<]"
syn match   earStruct   "[{}()[\]_~\\|]\|\(\s\)\@<==\(\s\)\@="
syn region  earString   start='"' skip='\\"' end='"'
syn keyword earKeyword  import
syn match   earComment  "#.*"

if version >= 508 || !exists("did_ear_syn_inits")
  if version < 508
    let did_ear_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink earNumber   Number
  HiLink earStruct   Structure
  HiLink earVariable Identifier
  HiLink earOperator Operator
  HiLink earString   String
  HiLink earKeyword  Statement
  HiLink earComment  Comment

  delcommand HiLink
endif

let b:current_syntax = "ear"

if main_syntax == 'ear'
  unlet main_syntax
endif
