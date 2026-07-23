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

" clipboard_indent.vim

if exists('g:loaded_clipboard_indent_plugin') || &compatible
  finish
endif
let g:loaded_clipboard_indent_plugin = 1

" Key mappings
nnoremap <silent> ,p :call clipboard_indent#paste_indent(1)<CR>
vnoremap <silent> ,c "+y:call clipboard_indent#yank_trim(1)<CR><Esc>
vnoremap <silent> ,d "+d:call clipboard_indent#yank_trim(1)<CR><Esc>
vnoremap <silent> ,D "+D:call clipboard_indent#yank_trim(1)<CR><Esc>
nnoremap <silent> ,d "+dd:call clipboard_indent#yank_trim(1)<CR><Esc>
nnoremap <silent> ,D "+D:call clipboard_indent#yank_trim(1)<CR><Esc>
