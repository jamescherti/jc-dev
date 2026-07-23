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

" terminal.vim

function! terminal#close_all() abort
  let l:buflist = getbufinfo()
  for l:buf in l:buflist
    if getbufvar(l:buf.bufnr, '&buftype', '') ==# 'terminal'
      execute 'bwipeout! ' . l:buf.bufnr
    endif
  endfor
endfunction

function! terminal#update_lastdir(path) abort
  let l:lastdir_filename = fnamemodify('~/.bash_lastdir', ':p')
  if filereadable(l:lastdir_filename)
    call writefile([a:path], l:lastdir_filename, 'w')
  endif
endfunction

function! terminal#tab(cmd, cwd, tabname, close) abort
  call unix#load_ssh_agent()

  call terminal#update_lastdir(a:cwd)

  execute '-tabnew'
  call chdir(a:cwd)


  let l:term_finish = a:close ? 'close' : 'open'

  call term_start(a:cmd, {'curwin': 1,
  \                       'term_name': a:tabname,
  \                       'term_finish': l:term_finish})
endfunction

function! terminal#tab_shell(...) abort
  let l:cmd = (len(a:000) > 0) ? a:000[0] : 'bash'
  let l:cwd = (len(a:000) > 1) ? a:000[1] : getcwd()
  let l:tabname = (len(a:000) > 2) ? a:000[2] : '<term>'

  let l:close = 1
  call terminal#tab(l:cmd, l:cwd, l:tabname, l:close)
endfunction

function! terminal#tmux() abort
  let l:terminal_name = '<tmux>'
  let l:tabpagenr = tabpagenr()

  " Switch to an existing terminal
  let l:bufnr = buffer#find_bufname(l:terminal_name)
  if l:bufnr >= 0
    execute 'bwipeout! ' . l:bufnr
  endif

  " New terminal
  if executable('tmux')
    let l:cmd = ['sh', '-c', 'tmux -2 attach || tmux -2']
  else
    let l:cmd = ['bash']
  endif

  let l:cwd = getcwd()

  let l:close = 1
  call terminal#tab(l:cmd, l:cwd, l:terminal_name, l:close)
  execute 'tabmove ' . tabpagenr() - 1

  let l:shell = 'bash'
  let l:tmux_run = 'tmux-run'
  call system(shellescape(l:tmux_run) . ' ' . shellescape(l:shell) . ' &')
endfunction

function! terminal#external_terminal_tmux_bash() abort
  let l:shell = 'bash'
  let l:tmux_run = 'tmux-run'
  let l:xdevenv = 'xdevenv'

  if ! executable(l:shell) || ! executable(l:tmux_run)
    return
  endif

  call terminal#update_lastdir(getcwd())
  call system(shellescape(l:tmux_run) . ' ' . shellescape(l:shell) . ' &')

  if executable(l:xdevenv)
    call system(l:xdevenv . ' terminal &')
  endif
endfunction

function! terminal#run_in_external_terminal(map, command) abort
  let command = a:command
  if has('gui_running')
    if executable('xterm')
      let command = 'xterm -e ' . shellescape(a:command) . ' &'
    endif
  endif

  call MapCommand(a:map, command)
endfunction
