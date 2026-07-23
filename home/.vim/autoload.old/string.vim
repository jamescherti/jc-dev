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

" string.vim

function! string#lstrip_whitespace(s) abort
    return substitute(a:s, '^\s\+', '', '')
endfunction

function! string#rstrip_whitespace(s) abort
    return substitute(a:s, '\s\+$', '', '')
endfunction

function! string#leading_whitespace(string) abort
  let l:remaining = string#lstrip_whitespace(a:string)
  let l:whitespace_len = len(a:string) - len(l:remaining)
  return a:string[0:l:whitespace_len - 1]
endfunction

function! string#strip(input_string) abort
  if ! exists('*trim')
    return substitute(a:input_string, '^\s*\(.\{-}\)\s*$', '\1', '')
  endif
  return trim(a:input_string)
endfunction

" moved to lightvim
function! string#rstrip(string, chars) abort
  let l:index = len(a:string) - 1
  while l:index >= 0
    let l:found = 0
    for l:char in split(a:chars, '\zs')
      if a:string[l:index] ==# l:char
        let l:found = 1
        break
      endif
    endfor

    let l:index -= 1

    if ! l:found
      break
    endif
  endwhile

  return a:string[0:l:index+1]
endfunction

function! string#lstrip(string, chars) abort
  let l:index = 0
  while l:index < len(a:string)
    let l:found = 0
    for l:char in a:chars
      if a:string[l:index] ==# l:char
        let l:found = 1
        break
      endif
    endfor

    if ! l:found
      break
    endif

    let l:index += 1
  endwhile

  " vint: next-line -ProhibitUsingUndeclaredVariable
  return a:string[l:index:]
endfunction

function! string#replace_home_with_tilde(path) abort
  let l:path = fnamemodify(a:path, ':p')

  let l:path_sep = (!exists('+shellslash') || &shellslash) ? '/' : '\\'
  let l:home = fnamemodify('~', ':p') . l:path_sep

  if l:path[0:len(l:home)-1] ==# l:home
    return '~' . l:path_sep . l:path[len(l:home):]
  elseif l:path == l:home
    return '~' . l:path_sep
  endif

  return l:path
endfunction
