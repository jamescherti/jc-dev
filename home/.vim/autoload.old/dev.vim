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

" dev.vim

" TODO: Python coverage
"

" Usage: call dev#config_python('path_to_python_project/module_name')
"        call dev#config_python(g:localvimrc_script_dir . 'dir/module_name')
function! dev#python_path(python_project_path) abort
  if !isdirectory(a:python_project_path)
    echoerr 'The Python project path does not exist: ' . a:python_project_path
    return
  endif

  let b:python_project_path =
    \ path#rstrip_sep(fnamemodify(a:python_project_path, ':p'))

  "-----------------------------
  " Ale
  "-----------------------------
  " let l:env_path =
  "   \ '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin'
  "   \        shellescape('VIRTUAL_ENV=""'),
  "   \        shellescape('PATH=' . l:env_path),
  " \        'PYTHONPATH=' .  substitute(l:pythonpath, '\(.\)', '\\\1', 'g'),
  let l:pythonpath =
    \ path#rstrip_sep($PYTHONPATH . (($PYTHONPATH !=# '') ? ':' : '') .
    \ b:python_project_path)

  let b:ale_shell = printf('env %s %s', 'PYTHONPATH=' . l:pythonpath, &shell)

  "-----------------------------
  " Jedi
  "-----------------------------
  let b:jedi_added_sys_path = get(b:, 'jedi_added_sys_path', [])
  if index(b:jedi_added_sys_path, b:python_project_path) < 0
    let b:jedi_added_sys_path += [b:python_project_path]
  endif

  "-----------------------------
  " Coverage
  "-----------------------------
  augroup PythonProjectHighlightCoverage
    autocmd! * <buffer>
    " autocmd!
    autocmd BufWritePost <buffer>
      \ if exists('b:python_project_path')
      \        && stridx(expand('%:p'), '/tests/') <# 0
      \        && filereadable(path#join(b:python_project_path, '.coverage')) |
      \   packadd coverage-highlight |
      \   silent HighlightCoverage |
      \ endif
  augroup END

  let b:project_path = a:python_project_path
endfunction

function! dev#lvimrc_python_path(subdir) abort
  call dev#python_path(path#join(g:localvimrc_script_dir, a:subdir))
endfunction

" Return true when a:name is a directory that is in the same directory as
" '.lvimrc'.
"
" Usage: dev#is_lvimrc_subdir('dir_in_the_same_dir_as_lvimrc')
function! dev#is_lvimrc_subdir(name) abort
  if ! exists('g:localvimrc_script')
    return 0
  endif

  let l:localvimrc_script_dir = fnamemodify(g:localvimrc_script, ':p:h')
  " return l:localvimrc_script_dir[0:len(l:localvimrc_script_dir)-1] ==# l:localvimrc_script_dir
  let l:subdir_path = path#join(l:localvimrc_script_dir, a:name)
  if isdirectory(l:subdir_path)
    return 1
  endif

  return 0
endfunction

function! dev#project_type(path) abort
  if isdirectory(path#join(a:path, 'colors')) ||
        \ isdirectory(path#join(a:path, 'plugin')) ||
        \ isdirectory(path#join(a:path, 'autoload'))
    return 'vim'
  elseif filereadable(path#join(a:path, 'setup.py')) ||
        \ filereadable(path#join(a:path, 'pyproject.toml'))
    return 'python'
  endif

  return 'unknown'
endfunction
