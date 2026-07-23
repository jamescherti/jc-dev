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

" Move the cursor to the beginning or end of a visual selected area.
" and keep the selection unchanged (unlike '< and '>).
function! folding#unfold_move_to_start_fold(unfold_mapping) abort
  let l:first_line_fold = foldclosed('.')
  let l:last_line_fold = foldclosedend('.')
  if l:first_line_fold !=# -1
    let l:pos = getpos('.')
    let l:pos[1] = l:first_line_fold
    call setpos('.', l:pos)
    execute 'normal! ' . a:unfold_mapping
    if l:first_line_fold !=# foldclosed('.') || l:last_line_fold !=# foldclosedend('.')
      normal! j[z
    endif
  endif
endfunction

" Prevent G from opening the last fold when 'set foldopen+=insert'
function! folding#goto_last_line() abort
  let l:last_line = line('$')
  execute ':' . l:last_line
endfunction

function! folding#foldtext_simple() abort
  let l:line = getline(v:foldstart)
  let l:sub = substitute(l:line, '/\*\|\*/\|{\{3\}\d\=', '', 'g')
  return repeat(v:folddashes[1:], 4) . l:sub
  return l:sub
endfunction

" https://habamax.github.io/2020/02/03/vim-folding-for-python.html
function! folding#foldexpr_indent() abort
  let indent = indent(v:lnum)/&sw
  let indent_next = indent(nextnonblank(v:lnum+1))/&sw
  if indent_next > indent && getline(v:lnum) !~ '^\s*$'
    return ">" . (indent+1)
  elseif indent != 0
    return indent
  else
    return -1
  endif
endfunction
