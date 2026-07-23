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

" grep.vim

if exists('g:loaded_grep_plugin') || &compatible
  finish
endif
let g:loaded_grep_plugin = 1

function! s:grep(opts, arg) abort
  let l:save_qflist = getqflist()
  call setqflist([])
  let l:arg = substitute(a:arg, '\(.\)', '\\\1', 'g')
  " let l:arg = escape(a:arg, '#%()')
  " execute 'silent grep ' . a:opts . ' ' . l:arg
  execute 'silent grep ' . a:opts . ' ' . l:arg
  let l:len_qflist = len(getqflist())

  if l:len_qflist !=# 0
    echo l:len_qflist . ' matches.'
  else
    echo 'No matches.'
    call setqflist(l:save_qflist)
  endif
endfunction

" command! -nargs=1 Grep call <SID>grep('', <f-args>)
" command! -nargs=1 IGrep call <SID>grep('-i', <f-args>)
" command! -nargs=1 FGrep call <SID>grep('-F', <f-args>)
" command! -nargs=1 IFGrep call <SID>grep('-Fi', <f-args>)
"
" function! s:escape_grep(arg) abort
"   " No need to escape for now (because command without -bar is used)
"   return a:arg
" endfunction
"
" nnoremap ,gr :Grep<Space>
" nnoremap ,igr :IGrep<Space>
" nnoremap ,igw :call feedkeys(':IFGrep ' . <SID>escape_grep(expand('<cword>')))<CR>
"
" nnoremap ,gy :FGrep<Space><C-r>=<SID>escape_grep(trim(@@))<CR>
" nnoremap ,gc :FGrep<Space><C-r>=<SID>escape_grep(trim(@"))<CR>
" vnoremap ,gr :<C-U>call feedkeys(':FGrep ' . <SID>escape_grep(substitute(visual#get_text(), '\n', '\n', 'g')), 'n')<CR>
" " vnoremap ,gr :<C-U>call feedkeys(':FGrep ' . <SID>escape_grep(substitute(visual#get_text(), '\n', '\n', 'g')), 'n')<CR>
" nnoremap ,gw :call feedkeys(':FGrep ' . <SID>escape_grep(expand('<cword>')), 'n')<CR>
"
" nnoremap ,gf :FGrep<Space>
" nnoremap ,igf :IFGrep<Space>
" " nnoremap ,gfp :FGrep<Space><C-r>=<SID>escape_grep(trim(@"))<CR>
" " nnoremap ,gfw :call feedkeys(':FGrep ' . <SID>escape_grep(expand('<cword>')))<CR>

nnoremap ,gp :silent grep<Space><C-r>=<SID>escape_grep(string#strip(@"))<CR>
nnoremap ,gw :let @/=expand('<cword>')<CR>:call feedkeys(':silent grep ' . shellescape(expand('<cword>')), 'n')<CR>
" nnoremap ,gw :let @/=expand('<cword>') | call feedkeys(':silent grep ' . shellescape(expand('<cword>')), 'n')
