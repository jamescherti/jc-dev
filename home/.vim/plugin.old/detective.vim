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

" detective.vim

if exists('g:loaded_indent_detective_plugin') || &compatible
  finish
endif
let g:loaded_indent_detective_plugin = 1

function! s:dict_get_highest(dictionary) abort
  let l:highest = -1
  let l:highest_key = ''
  for l:indent_type in keys(a:dictionary)
    if a:dictionary[l:indent_type] ># l:highest
      let l:highest = a:dictionary[l:indent_type]
      let l:highest_key = l:indent_type
    endif
  endfor

  return [l:highest, l:highest_key]
endfunction

function! s:dict_values_sum(dictionary) abort
  let l:sum = 0
  for l:key in keys(a:dictionary)
    let l:sum += a:dictionary[l:key]
  endfor
  return l:sum
endfunction

function! s:dict_values_max(dictionary) abort
  let l:max = -1
  let l:max_key = ''

  for l:key in keys(a:dictionary)
    if l:max <# 0 || a:dictionary[l:key] > l:max
      let l:max = a:dictionary[l:key]
      let l:max_key = l:key
    endif
  endfor

  return l:max_key
endfunction

function! s:indent_detective_echo(string) abort
  let b:indent_detective_msg = a:string
  echo printf('Indent Detective: %s', b:indent_detective_msg)
endfunction

function! s:indent_detective() abort
  let b:indent_detective_msg = ''

  if !empty(&buftype)
    return
  endif

  let l:max_indent_to_check = 128
  let l:dict_indent_spaces = {}
  let l:dict_indent_tabs = {}

  for l:line in getline('^', '$')
    let l:line = string#rstrip(l:line, "\t ")
    if match(string#strip(l:line), '^[A-Za-z_]') <# 0
      " The line has to start with a letter or '_'
      " (Because instructions usually start with letters)
      continue
    endif

    let l:indent = indenthelper#get_indent(l:line)
    if l:indent ==# ''
      " Ignore when there is no indentation
      continue
    endif

    if stridx(l:indent, "\t") !=# -1 && stridx(l:indent, ' ') !=# -1
      " Ignore when there is a mix of tabs and spaces
      continue
    endif

    if l:indent[0] ==# "\t"
      " There is no need to store more than one Tab
      let l:indent = "\t"

      if !has_key(l:dict_indent_tabs, l:indent)
        let l:dict_indent_tabs[l:indent] = 1
      endif
      let l:dict_indent_tabs[l:indent] += 1
    else
      " Ignore odd numbers
      if len(l:indent) % 2 ==# 1
        continue
      endif

      if !has_key(l:dict_indent_spaces, l:indent)
        let l:dict_indent_spaces[l:indent] = 1
      endif
      let l:dict_indent_spaces[l:indent] += 1
    endif

    " echo l:line

    let l:max_indent_to_check -= 1
    if l:max_indent_to_check <# 0
      break
    endif
  endfor

  " echomsg 'Stat: ' . string(l:dict_indent_spaces)

  " Find the biggest index type
  let [l:highest_indent_type,  l:highest_indent_type_key] =
        \ s:dict_get_highest(l:dict_indent_spaces)

  if l:highest_indent_type <# 0
    call s:indent_detective_echo('<undetected>')
    return
  endif

  " Space-based indentation
  " echomsg 'Space-based:' .
  "         \ string(s:dict_values_sum(l:dict_indent_spaces)) .
  "         \ ' Tab-based:' .
  "         \ string(s:dict_values_sum(l:dict_indent_tabs))
  if s:dict_values_sum(l:dict_indent_spaces) >#
        \ s:dict_values_sum(l:dict_indent_tabs)
    " Find the one that can device all others
    let l:how_many_can_be_divided = {}
    for l:indent_type in keys(l:dict_indent_spaces)
      for l:indent_type2 in keys(l:dict_indent_spaces)
        if len(l:indent_type2) % len(l:indent_type) ==# 0
          if !has_key(l:how_many_can_be_divided, l:indent_type)
            let l:how_many_can_be_divided[l:indent_type] = 0
          endif

          let l:how_many_can_be_divided[l:indent_type] += 1
        endif
      endfor
    endfor
    " echomsg 'Greatest dividor: ' . string(l:how_many_can_be_divided)

    let l:detected_indent = len(s:dict_values_max(l:how_many_can_be_divided))

    " Set
    setlocal expandtab
    let &l:tabstop = l:detected_indent
    let &l:softtabstop = l:detected_indent
    let &l:shiftwidth = l:detected_indent

    echo b:indent_detective_msg
    call s:indent_detective_echo(l:detected_indent . ' spaces')
    return

  " Tab-based indentation
  else
    setlocal noexpandtab preserveindent copyindent
    setlocal softtabstop=0 shiftwidth=4 tabstop=4
    call s:indent_detective_echo('Tab-based indentation')
    return
  endif

  call s:indent_detective_echo('Nothing to do.')
endfunction

function! s:autocmd_indent_detective() abort
  if get(b:, 'indent_detective_enabled', get(g:, 'indent_detective_enabled', 1))
    if len(get(g:, 'securemodelines_modified', [])) > 0
      echoerr 'Indent: Ignored because the modeline has been modified'
      return
    endif

    IndentDetective
  endif
endfunction

command! -bar IndentDetective call <SID>indent_detective()

augroup IndentDetective
  autocmd!
  autocmd BufReadPost * silent call s:autocmd_indent_detective()
augroup END
