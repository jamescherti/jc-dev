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

" folding.vim

if exists('g:loaded_folding_plugin') || &compatible
  finish
endif
let g:loaded_folding_plugin = 1

set foldignore=
set foldmethod=marker
set foldenable

"
" Open fold and move to the beginning
"

set foldopen+=hor
set foldopen+=jump
nnoremap <silent> zO :call folding#unfold_move_to_start_fold('zO')<CR>
nnoremap <silent> zo :call folding#unfold_move_to_start_fold('zo')<CR>
nnoremap <silent> <Space> :call folding#unfold_move_to_start_fold('zo')<CR><Space>
" nnoremap <silent> l :call <SID>unfold_move_to_start_fold('zo')<CR>l
" nnoremap <silent> h :call <SID>unfold_move_to_start_fold('zo')<CR>h

set foldopen+=insert
nnoremap <silent> G :call folding#goto_last_line()<CR>

" Better folding
set foldtext=folding#foldtext_simple()
