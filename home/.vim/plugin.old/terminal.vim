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

" terminal.vim

if exists('g:loaded_terminal_plugin') || &compatible
  finish
endif
let g:loaded_terminal_plugin = 1

tnoremap <C-S-g> <C-w>:call terminal#tab_shell()<CR>
nnoremap <silent> ,B :call terminal#tab_shell()<CR>
nnoremap <silent> ,fr :call terminal#tab_shell('ranger .', getcwd(), '<ranger>')<CR>
nnoremap <silent> ,eT :call terminal#external_terminal_tmux_bash()<CR>
" nnoremap <silent> ,et :call terminal#tmux()<CR>

nnoremap <silent> <M-CR> :call terminal#tmux()<CR>
