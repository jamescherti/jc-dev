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

" python.vim

" Add import 'module_name' to the source code
function! python#add_import(module_name) abort
  if ! executable('isort')
    echomsg 'Error: the command "isort" was not found.'
    return
  endif

  if &filetype !=# 'python'
    echomsg 'Error: the filetype has to be Python.'
    return
  endif

  silent write

  let l:list_lines1 = readfile(expand('%'), 1)
  let l:line = line('.')
  let l:column = col('.')
  let l:num_lines = line('$')

  call system('isort --add-import ' . shellescape(a:module_name) . ' ' . shellescape(expand('%')))
  let l:list_lines2 = readfile(expand('%'), 1)

  silent edit %

  let l:line = l:line + (line('$') - l:num_lines)
  call cursor(l:line, l:column)
endfunction
