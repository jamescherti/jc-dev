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

" visual.vim

"
" Indent + move to the beginning of a selection
"
function! visual#move_to_beginning() abort
  let l:line1 = line('.')
  normal! o
  let l:line2 = line('.')
  if l:line2 > l:line1
    normal! o
  endif

  if line('.') > 1
    " It is always useful to show to the user the line that is above the
    " visual.
    normal! kj
  endif

  if visualmode() ==# 'V'
    normal! ^
  endif
endfunction

"
" Indent + move to the end of a selection
"
function! visual#move_to_end() abort
  let l:line1 = line('.')
  normal! o
  let l:line2 = line('.')
  if l:line2 < l:line1
    normal! o
  else
    if visualmode() ==# 'V'
      normal! $
    endif
  endif
endfunction

"
" Visual indent
"
function! visual#indent(indent_type) abort
  let b:visual_mode_move_to_start = get(b:, 'visual_mode_move_to_start', 1)
  if a:indent_type ==# 0
    normal! <gv
  else
    normal! >gv
  endif

  if b:visual_mode_move_to_start
    call visual#move_to_beginning()
    let b:visual_mode_move_to_start = 0
  endif
endfunction

"
" Get the content of a visual
"
function! visual#get_text() abort
  let [line_start, column_start] = getpos("'<")[1:2]
  let [line_end, column_end] = getpos("'>")[1:2]
  let lines = getline(line_start, line_end)
  if len(lines) == 0
    return ''
  endif
  let lines[-1] = lines[-1][: column_end - (&selection ==# 'exclusive' ? 2 : 1)]
  let lines[0] = lines[0][column_start - 1:]
  return join(lines, "\n")
endfunction

function! visual#get_text_using_registers() abort
  try
    let l:register_save = @z

    if mode() ==# 'n'
      normal! gv
    endif

    normal! "zy

    let l:register_value = @z
    return l:register_value
  finally
    let @z = l:register_save
  endtry
endfunction

function! visual#modify(text) range
  " Reselect
  if mode() ==# 'n'
    normal! gv
  endif

  " Delete
  normal! "_d

  try
    " Save register
    let l:register_save = @z
    let @z = a:text

    " Paste
    normal! "zP

    " Select
    call vim#select_pasted_text()
  finally
    " Restore register
    let @z = l:register_save
  endtry
endfunction

function! visual#sort_comma_separated() range
  let l:visual_content =
    \ join(
    \   sort(
    \     split(visual#get_text_using_registers(), ',')
    \   ),
    \ ',')

  call visual#modify(l:visual_content)
endfunction
