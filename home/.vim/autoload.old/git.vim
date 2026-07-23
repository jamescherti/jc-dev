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

" git.vim

function! git#get_diff_lines() abort
  let l:result = []
  let l:list_lines = systemlist(
    \ 'git -C ' . shellescape(expand('%:h')) .
    \ ' diff --no-ext-diff --no-color --unified=0 ' .
    \ shellescape(expand('%:p')))

  if v:shell_error !=# 0
    echomsg 'Error: git diff: ' . join(l:list_lines, "\n")
    return []
  endif

  for l:line in l:list_lines
    if match(l:line, '^@@ -[0-9,]\+ +[0-9,]\+ @@') >= 0
      let l:line =
        \ substitute(l:line, '^\(@@ -[0-9,]\+ +[0-9,]\+ @@\).*', '\1', '')
      let [l:before, l:after] = split(l:line[3:-3])
      if l:before[0] !=# '-'
        continue
      endif

      if l:after[0] !=# '+'
        continue
      endif

      let l:after_split = split(l:after[1:], ',')

      let l:line_where_added = l:after_split[0]

      let l:num_lines_added = 1
      if len(l:after_split) > 1
        let l:num_lines_added = l:after_split[1]
      endif

      let l:range_lines =
        \ range(l:line_where_added, l:line_where_added + l:num_lines_added - 1)
      for l:line_num in l:range_lines
        call add(l:result, l:line_num)
      endfor
    endif
  endfor

  return l:result
endfunction

function! git#run_git_in_buffer(cmd) abort
  " rightbelow vertical new
  tabnew
  execute 'read!' . a:cmd
  normal! ggdd

  call buffer#set_nofile()
  call buffer#set_nomodifiable()
  setlocal filetype=git
  setlocal syntax=git
endfunction

function! git#toplevel(...) abort
  if !executable('git')
    return ''
  endif

  let l:git_args = ''
  if len(a:000) > 0
    let l:git_args = '-C ' . shellescape(a:1) . ' '
  endif

  let l:git_output =
    \ systemlist(printf('git %srev-parse --show-toplevel', l:git_args))
  if v:shell_error !=# 0
    return ''
  endif

  let l:git_toplevel = l:git_output[0]
  if !isdirectory(l:git_toplevel)
    return ''
  endif

  return l:git_toplevel
endfunction

function! git#buffer_chdir_toplevel() abort
  let l:filepath = resolve(expand('%:p'))
  let l:dirname_filepath = fnamemodify(l:filepath, ':h')

  let l:gitdir = git#toplevel(l:dirname_filepath)
  if l:gitdir !=# '' && isdirectory(l:gitdir)
    call chdir(l:gitdir)
    " echo l:gitdir
  else
    echomsg 'Cannot detect the Git repository of: ' . l:filepath
  endif

  if &filetype ==# 'netrw' || &filetype ==# 'dirvish'
    execute ':edit ' . l:dirname_filepath
  endif
endfunction

function! git#branch() abort
  if !executable('git')
    return ''
  endif

  let l:lines = systemlist(
        \ printf('git -C %s symbolic-ref --short -q HEAD',
        \        shellescape(expand('%:p:h'))))
  if v:shell_error ==# 0 && len(l:lines) > 0
    return l:lines[0]
  endif

  return ''
endfunction

function! git#run_from_repo(cmd) abort
  let l:cwd = git#toplevel()
  if l:cwd ==# ''
    echoerr printf('The directory ''%s'' is not a Git repository', expand('%:p:h'))
  endif

  let l:close = 0
  call terminal#tab(a:cmd, l:cwd, '<ci>', l:close)
endfunction

function! git#find(...) abort
  if len(a:000) > 0
    let l:fnmatch = a:000[0]
  else
    let l:fnmatch = '**'
  endif

  if &modified
    echoerr 'No write since last change.'
    return
  endif

  if !executable('git')
    echomsg 'Warning: no git in $PATH'
    call unix#find(l:fnmatch)
    return
  endif

  if git#toplevel() ==# ''
    call unix#find(l:fnmatch)
    return
  endif

  let l:cmd = 'git ls-files -- ' . shellescape(l:fnmatch)
  let l:list_lines = systemlist(l:cmd)
  if v:shell_error !=# 0
    echomsg 'Git: ' . join(l:list_lines, "\n")
    return
  endif

  let l:list_files = []
  for l:filename in l:list_lines
    if filereadable(l:filename)
      call add(l:list_files, l:filename)
    endif
  endfor

  if len(l:list_files) ==# 0
    echo 'No Git files were found in the directory ' . getcwd()
    return
  endif

  call vim#cfile_list(l:list_files)
endfunction
