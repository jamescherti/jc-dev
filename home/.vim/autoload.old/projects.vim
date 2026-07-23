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

" projects.vim
" Requires: tab.vim, autocomplete.vim

"
" Projects
"
function! projects#load(filename) abort
  execute 'silent source ' . fnameescape(a:filename)
endfunction

function! projects#choose() abort
  call complete#autocomplete_files(g:vim_projects_dir, '*.vim', function('projects#load'), '.vim')
endfunction

" Open each file in a new tab
function! projects#helper_tabs(dir, files) abort
  for l:item in a:files
    if !tab#is_last()
      tabnew
    endif

    execute 'lcd ' . fnameescape(a:dir)
    execute 'edit ' . fnameescape(l:item)
  endfor
  execute 'tabprevious ' . string(len(a:files) - 1)
endfunction

" Open each file in a vertical split
function! projects#helper_vsplit(dir, files) abort
  tabnew

  let l:first = 1
  for l:item in a:files
    if l:first ==# 0
      vsplit
      wincmd l
    endif
    let l:first = 0

    execute 'lcd ' . fnameescape(a:dir)
    execute 'edit ' . fnameescape(l:item)
  endfor

  let i = 0
  while i < len(a:files) - 1
    wincmd h
    let i += 1
  endwhile
endfunction

"
" Helpers
"
" function! projects#helpers#load(filename) abort
"   execute 'silent source ' . fnameescape(a:filename)
" endfunction
"
" function! projects#helpers#choose() abort
"   echo 'Helpers:'
"   echo '---------'
"   echo
"   call complete#autocomplete_files(g:vim_helpers_dir, '*.vim', function('VimHelpersLoad'))
" endfunction
