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

" autocomplete.vim
" Requires: string.vim

" directory: path to the directory where the files are
" file_pattern: pattern like '*.vim'
" function_load_project: the function that will be executed if a file is chosen
" hide_ext: the extension that should be hidden (e.g. '.vim')
function! complete#autocomplete_files(directory, file_pattern, function_load_project, hide_ext) abort
  function! AutoComplete(arglead, cmdline, cursorpos) abort
    let l:completion_list = []
    for l:item in w:_autocomplete_files_completion_list
      if a:cmdline ==# '' || a:cmdline ==# l:item[0:len(a:cmdline) -1]
        call add(l:completion_list, l:item)
      endif
    endfor
    return l:completion_list
  endfunction

  while 1
    let w:_autocomplete_files_completion_list = []
    for path in sort(split(globpath(a:directory, a:file_pattern), '\n'))
      if a:hide_ext ==# ''
        let l:file_name = fnamemodify(path, ':t')
      else
        let l:file_name = fnamemodify(path, ':t:r')
      endif

      call add(w:_autocomplete_files_completion_list, l:file_name)
      echo l:file_name
    endfor

    echo "\n"
    let l:file_name = string#strip(input('Choice: ', '', 'customlist,AutoComplete'))
    unlet w:_autocomplete_files_completion_list
    if l:file_name ==# ''
      redraw!
      break
    endif

    let l:path_sep = (!exists('+shellslash') || &shellslash) ? '/' : '\\'

    let l:file_path = a:directory . l:path_sep . l:file_name
    let l:file_path = fnamemodify(l:file_path, ':p')
    if a:hide_ext !=# ''
      let l:file_path .= a:hide_ext
    endif
    if filereadable(l:file_path)
      try
        call a:function_load_project(l:file_path)
      finally
        break
      endtry
    else
      echo 'Error: "' . l:file_name . '" does not exist.'
    endif
  endwhile

  redraw!
endfunction
