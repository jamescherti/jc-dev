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

" path.vim

" Join two or more path components, inserting '/' as needed.
function! path#join(path, ...) abort
  let l:path = path#rstrip_sep(a:path)
  for l:item in a:000
    let l:path = l:path . path#sep() . path#rstrip_sep(l:item)
  endfor
  return l:path
endfunction

function! path#sep() abort
  return (!exists('+shellslash') || &shellslash) ? '/' : '\'
endfunction

function! path#rstrip_sep(path) abort
  return string#rstrip(a:path, path#sep())
endfunction

function! path#cwd_startswith(prefix) abort
  let l:cwd = expand('%:p')
  if l:cwd[0:len(a:prefix) - 1] ==# a:prefix
    return 1
  endif
  return 0
endfunction

" Return the file name of one of the files in the list 'a:list_file_names'
" that exist in the directory 'a:dir'.
"
" Return an empty if none of the files was found in 'a:dir'.
"
" Example:
" --------
" :echo path#one_of_files_exist('.', ['.git/', 'setup.py'])
" /full/path/to/.git
function! path#one_of_files_exist(dir, list_file_names) abort
  let l:cur_dir = path#rstrip_sep(fnamemodify(a:dir, ':p'))
  if !isdirectory(l:cur_dir)
    return ''
  endif

  for l:file_name in a:list_file_names
    let l:is_dir = 0
    if len(l:file_name) > 0 && l:file_name[-1:-1] ==# '/'
      let l:file_name = path#rstrip_sep(l:file_name)
      let l:is_dir = 1
    endif

    let l:path_file_name = l:cur_dir . path#sep() . l:file_name

    if l:is_dir
      if isdirectory(l:path_file_name)
        return l:path_file_name
      endif
    else
      if filereadable(l:path_file_name)
        return l:path_file_name
      endif
    endif
  endfor

  return ''
endfunction
