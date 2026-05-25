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
" THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
" SOFTWARE.

" clipboard.vim

if exists('g:loaded_clipboard_plugin') || &compatible
  finish
endif
let g:loaded_clipboard_plugin = 1

" Copy / paste
function! s:paste_excluding_newline(text, mode) abort
  let l:save_register = @@
  try
    let @@ = string#rstrip(a:text, "\n")
    if a:mode ==# 'visual' || col('.') ==# 1
      normal! P
    else
      normal! p
    endif

    call vim#select_pasted_text()

    if a:mode ==# 'insert'
      normal! `]
      if &selection ==# 'exclusive'
        normal! l
      endif
    endif
  finally
    let @@ = l:save_register
  endtry
endfunction

if !exists('+clipboard') ||
        \ (empty($DISPLAY) && len(exepath('xclip')) &&
        \   len(exepath('pbcopy')) && len(exepath('pbpaste')))
  "
  " xclip clipboard
  "
  function! s:pbcopy_visual() abort
    call system('pbcopy', visual#get_text_using_registers())
    " normal! gv
  endfunction

  vnoremap <C-c> :<C-U>call <SID>pbcopy_visual()<CR>
  vnoremap <C-x> :<C-U>call <SID>pbcopy_visual()<CR>gv"_d

  function! s:get_from_clipboard() abort
    return system('pbpaste')
  endfunction
else
  "
  " Vim clipboard
  "
  function! s:get_from_clipboard() abort
    return @+
  endfunction

  vnoremap <C-c> "+y
  vnoremap <C-x> "+c
  vnoremap <C-v> "+p
endif

" "vnoremap <C-v> "_d<Esc>:<C-U>call <SID>paste_excluding_newline(<SID>get_from_clipboard(), 'visual')<CR><Esc>
" "inoremap <C-v> <Esc>:call <SID>paste_excluding_newline(<SID>get_from_clipboard(), 'insert')<CR><Esc>i
