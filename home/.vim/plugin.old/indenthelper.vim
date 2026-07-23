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

" indenthelper.vim

if exists('g:loaded_indenthelper_plugin') || &compatible
  finish
endif
let g:loaded_indenthelper_plugin = 1

inoremap <expr> <S-Tab> indenthelper#smart_indent()

" Hack to preserve indentation
" Bug with the mappings: the cursor has been moving from the text to the
" tabline. That is why I disabled it.

function! s:add_empty_line_before() abort
  let l:indent = indenthelper#get_indent(getline('.'))
  normal! O
  if string#strip(getline('.')) ==# ''
    call setline(line('.'), l:indent)

    let l:pos = getpos('.')
    let l:pos[2] = len(l:indent) + 1
    call setpos('.', l:pos)
  endif

  startinsert
endfunction

nnoremap <silent> o o<Space><BS>
" nnoremap <silent> <silent> O :call <SID>add_empty_line_before()<CR>
nnoremap <silent> O O<Space><BS>
