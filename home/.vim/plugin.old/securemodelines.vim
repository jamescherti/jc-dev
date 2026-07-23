" Author: James Cherti
" URL: https://github.com/jamescherti/jc-dev
"
" Distributed under terms of the MIT license.
"
" Copyright (C) 2000-2026 James Cherti
"
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the “Software”), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
"
" The above copyright notice and this permission notice shall be included in all
" copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
" SOFTWARE.

" Script:           securemodelines.vim
" Version:          2022-12-27
" Maintainer:       JC
" Requires:         Vim >= 7
" License:          Redistribute under the same terms as Vim itself
" Purpose:          A secure alternative to modelines
" Based On:         securemodelines.vim
" OriginalAuthor:   Ciaran McCreesh <ciaranm@ciaranm.org>
" Homepage:         http://ciaranm.org/tag/securemodelines
"                   https://www.vim.org/scripts/script.php?script_id=1876

if exists('g:loaded_securemodelines_plugin') || v:version < 700 || &compatible
  finish
endif
let g:loaded_securemodelines_plugin = 1

if !exists('g:securemodelines_allowed_items')
  let g:securemodelines_allowed_items = [
        \ 'textwidth',   'tw',
        \ 'softtabstop', 'sts',
        \ 'tabstop',     'ts',
        \ 'shiftwidth',  'sw',
        \ 'expandtab',   'et',  'noexpandtab', 'noet',
        \ 'filetype',    'ft',
        \ 'foldmethod',  'fdm',
        \ 'readonly',    'ro',  'noreadonly', 'noro',
        \ 'rightleft',   'rl',  'norightleft', 'norl',
        \ 'spell',
        \ 'spelllang'
        \ ]
endif

if (!exists('g:securemodelines_verbose'))
  let g:securemodelines_verbose = 0
endif

if (!exists('g:securemodelines_modelines'))
  let g:securemodelines_modelines=5
endif

if (!exists('g:securemodelines_leave_modeline'))
  if &modeline
    set nomodeline
    if g:securemodelines_verbose
      echohl WarningMsg
      echomsg 'Forcibly disabling internal modelines for securemodelines.vim'
      echohl None
    endif
  endif
endif

function! <SID>IsInList(list, i) abort
  for l:item in a:list
    if a:i == l:item
      return 1
    endif
  endfor
  return 0
endfun

function! <SID>DoOne(item) abort
  let l:matches = matchlist(a:item, '^\([a-z]\+\)\%(=[a-zA-Z0-9_\-.]\+\)\?$')
  if len(l:matches) > 0
    if index(g:securemodelines_allowed_items, l:matches[1]) >= 0
      exec 'setlocal ' . a:item

      let b:securemodelines_modified =
            \ get(b:, 'securemodelines_modified', [])
      call add(b:securemodelines_modified, matches[1])
    elseif g:securemodelines_verbose
      echohl WarningMsg
      echomsg "Ignoring '" . a:item . "' in modeline"
      echohl None
    endif
  endif
endfun

function! <SID>DoNoSetModeline(line) abort
  for l:item in split(a:line, '[ \t:]')
    call <SID>DoOne(l:item)
  endfor
endfun

function! <SID>DoSetModeline(line) abort
  for l:item in split(a:line)
    call <SID>DoOne(l:item)
  endfor
endfun

function! <SID>CheckVersion(op, ver) abort
  if a:op ==# '='
    return v:version != a:ver
  elseif a:op ==# '<'
    return v:version < a:ver
  elseif a:op ==# '>'
    return v:version >= a:ver
  else
    return 0
  endif
endfun

function! <SID>DoModeline(line) abort
  let l:matches = matchlist(
        \ a:line,
        \ '\%(\S\@<!\%(vi\|vim\([<>=]\?\)\([0-9]\+\)\?\)\|\sex\):\s*' .
        \ 'set\?\s\+\([^:]\+\):\S\@!')
  if len(l:matches) > 0
    let l:operator = '>'
    if len(l:matches[1]) > 0
      let l:operator = l:matches[1]
    endif
    if len(l:matches[2]) > 0
      if <SID>CheckVersion(l:operator, l:matches[2]) ? 0 : 1
        return
      endif
    endif
    return <SID>DoSetModeline(l:matches[3])
  endif

  let l:matches = matchlist(
        \ a:line,
        \ '\%(\S\@<!\%(vi\|vim\([<>=]\?\)\([0-9]\+\)\?\)\|\sex\):\(.\+\)'
        \ )
  if len(l:matches) > 0
    let l:operator = '>'
    if len(l:matches[1]) > 0
      let l:operator = l:matches[1]
    endif
    if len(l:matches[2]) > 0
      if <SID>CheckVersion(l:operator, l:matches[2]) ? 0 : 1
        return
      endif
    endif
    return <SID>DoNoSetModeline(l:matches[3])
  endif
endfun

function! <SID>DoModelines() abort
  if line('$') > g:securemodelines_modelines
    let l:lines={ }
    " TODO:
    " vint: next-line -ProhibitEqualTildeOperator
    call map(filter(getline(1, g:securemodelines_modelines) +
          \         getline(line('$') - g:securemodelines_modelines, '$'),
          \         'v:val =~ ":"'
          \  ),
          \  'extend(l:lines, { v:val : 0 } )')
    for l:line in keys(l:lines)
      call <SID>DoModeline(l:line)
    endfor
  else
    for l:line in getline(1, '$')
      call <SID>DoModeline(l:line)
    endfor
  endif
endfun

function! SecureModelines_DoModelines() abort
  call <SID>DoModelines()
endfun

augroup SecureModeLines
  autocmd!
  autocmd BufRead * :call <SID>DoModelines()
augroup END
