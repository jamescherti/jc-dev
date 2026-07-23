" Author: James Cherti
" URL: https://github.com/jamescherti/jc-dev
"
" Distributed under terms of the MIT license.
"
" Copyright (C) 2000-2026 James Cherti
"
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
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

" retab.vim
" http://vim.wikia.com/wiki/Super_retab

" Return indent (all whitespace at start of a line), converted from
" tabs to spaces if what = 1, or from spaces to tabs otherwise.
" When converting to tabs, result has no redundant spaces.
function! retab#indenting(indent, what, cols) abort
  let spccol = repeat(' ', a:cols)
  let result = substitute(a:indent, spccol, '\t', 'g')
  let result = substitute(result, ' \+\ze\t', '', 'g')
  if a:what ==# 1
    let result = substitute(result, '\t', spccol, 'g')
  endif
  return result
endfunction

function! retab#indent_convert(line1, line2, what, cols) abort
  " Convert whitespace used for indenting (before first non-whitespace).
  " what = 0 (convert spaces to tabs), or 1 (convert tabs to spaces).
  " cols = string with number of columns per tab, or empty to use 'tabstop'.
  " The cursor position is restored, but the cursor will be in a different
  " column when the number of characters in the indent of the line is changed.
  let savepos = getpos('.')
  let cols = empty(a:cols) ? &tabstop : a:cols
  keeppatterns execute a:line1 . ',' . a:line2 . 's/^\s\+/\=retab#indenting(submatch(0), a:what, cols)/e'
  " call histdel('search', -1)
  call setpos('.', savepos)
endfunction

function! retab#indent_spaces_4_to_2() abort
  let l:pos = getpos('.')
  setlocal tabstop=2 softtabstop=2 shiftwidth=2
  " vint: next-line -ProhibitCommandRelyOnUser -ProhibitCommandWithUnintendedSideEffect
  keeppatterns silent! %substitute/\v\t/  /g
  " vint: next-line -ProhibitCommandRelyOnUser -ProhibitCommandWithUnintendedSideEffect
  keeppatterns silent! %substitute;^\(\s\+\);\=repeat(' ', len(submatch(0))/2);g
  call setpos('.', l:pos)
endfunction
