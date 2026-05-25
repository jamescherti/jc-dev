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

" clipboard_indent.vim

function! s:clipboard_trim_space(string) abort
  " Find the shortest space
  let shortest_space = 0
  for line in split(a:string, "\n")
    let index = 0
    while index <= len(a:string)
      if a:string[index] !=# ' ' && a:string[index] !=# "\t"
        break
      endif
      let index = index + 1
    endwhile
    if shortest_space < index
      let shortest_space = index
    endif
  endfor

  " Modify the string
  let result = ''
  for line in split(a:string, "\n")
    let index = 0
    if len(line) >= shortest_space
      let index = shortest_space
    endif

    " vint: next-line "-ProhibitUsingUndeclaredVariable"
    let result = result . line[index:] . "\n"
  endfor

  return result
endfunction

function! clipboard_indent#yank_trim(trim_spaces) abort
  if a:trim_spaces
    let l:content = s:clipboard_trim_space(@@)
  else
    let l:content = @@
  endif

  let @+ = string#rstrip(content, "\n")
  let @@ = string#rstrip(l:content, "\n")
endfunction

function! clipboard_indent#paste_indent(respect_indent) abort
  if has('win32unix')
    let l:content = join(readfile('/dev/clipboard'), "\n")
  else
    let l:content = @+
  endif

  let l:content = s:clipboard_trim_space(l:content)

  if a:respect_indent
    let l:indent = indenthelper#get_indent(getline('.'))
    " let l:indent = indenthelper#smart_indent()
    let l:content = indenthelper#indent_string(l:content, l:indent)
  endif

  let l:save_register = @@
  let @@ = l:content
  normal! p
  let @@ = l:save_register
endfunction
