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

" indenthelper.vim

function! indenthelper#indent_string(multiline_string, prefix) abort
  let l:result = ''
  let l:first = 1

  for l:line in split(a:multiline_string, "\n", 1)
    if l:first ==# 1
      let l:first = 0
      let l:result .= l:line
    else
      let l:result .= "\n" . a:prefix . l:line
    endif
  endfor

  if a:multiline_string[-1] ==# "\n"
    let l:result = l:result . "\n"
  endif

  return l:result
endfunction

function! indenthelper#get_indent(string) abort
  " Return the indentation of a string
  let l:result = ''
  for l:char in split(a:string, '\zs')
    if l:char !~# '\s'
      break
    endif
    let l:result = l:result . l:char
  endfor
  return l:result
endfunction

" <expr> indent functions
function! indenthelper#smart_indent() abort
  let l:line = line('.')
  let l:col = col('.')
  let l:prev_line = indenthelper#prev_non_blank_from_column(l:line - 1, l:col)

  return indenthelper#get_indent(l:prev_line[l:col - 1:])
endfunction

function! indenthelper#prev_non_blank_from_column(line, col) abort
  " Find the previous line that is non blank starting from the current column
  let l:prev_line = ''
  let l:line = a:line
  while l:line > 0
    let l:curline = getline(l:line)
    if l:curline[a:col - 1:] =~# '[^\s]'
      let l:prev_line = l:curline
      break
    endif

    let l:line -= 1
  endwhile

  return l:prev_line
endfunction
