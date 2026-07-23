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

" Neovim does not require Fixkey.  If Neovim is detected, quietly bail out
" to smooth the user experience for users that switch between Vim and Neovim.
if exists("loaded_fixkey") || has('nvim') || has("gui_running")
    finish
endif
let loaded_fixkey = 1

" Alternative version:
" --------------------
" Description: Experimental function that fixes ALT key mappings in
"              terminal Vim.
"              Here are the two remaining issues:
"                - It breaks macros that use: '<Esc> + char'.
"                - Mappings that use '<Esc> + char' will not work anymore.
" Original author: Bruno Sutic (plugin: vim-alt-mappings)
"                  https://github.com/vim-utils/vim-alt-mappings
function! s:fix_alt_key_mappings() abort
  if has('gui_running') || has('nvim')
    return
  endif

  set ttimeout
  set ttimeoutlen=5

  let l:ascii_nums = [33] + range(35, 61) + range(63, 78) +
    \ range(80, 90) + range(92, 123) + [125, 126]
  for l:char in map(l:ascii_nums, 'nr2char(v:val)')
    execute 'set <M-' . l:char . ">=\<Esc>" . l:char
  endfor

  execute 'set <M-\">=' . nr2char(27) . '\"'
  execute 'set <M-bar>=' . nr2char(27) . '\|'
endfunction

if $TERM != 'screen-256color'
  finish
endif

call s:fix_alt_key_mappings()
finish
