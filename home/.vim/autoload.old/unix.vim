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

" unix.vim

" Helpers {{{

function! s:smart_mv_or_cp_path(name_or_path, is_path) abort
  if a:is_path
    " It is a path
    if isdirectory(a:name_or_path)
      let l:newfile = a:name_or_path . path#sep() . fnamemodify(bufname(), ':p:t')
    else
      let l:newfile = a:name_or_path
    endif
  else
    " It is a file name
    let l:newfile = expand('%:p:h') . path#sep() . a:name_or_path
  endif
  return l:newfile
endfunction

" }}}

" MIME type {{{

function! unix#xdg_open(file, ...) abort
  let l:wipeout = 0
  if len(a:000) > 0 && a:000[0] !=# 0
    let l:wipeout = 1
  endif

  if &modified && !l:wipeout
    if &modifiable
      write
    endif

    if &modified
      echoerr printf('Cannot save the buffer %s', bufnr(''))
      return 1
    endif
  endif

  let l:xdg_open_bin = 'xdg-open'
  let l:output = system(l:xdg_open_bin . ' ' . shellescape(a:file) . ' &')
  if v:shell_error !=# 0
    echomsg 'Error: ' . l:xdg_open_bin . ': ' . l:output
    return
  endif

  if l:wipeout
    bwipeout
  endif
endfunction

function! unix#mimetype(filename) abort
  if !executable('file')
    throw 'No ''file'' command in ' . $PATH
  endif

  let l:output = systemlist('file --mime-type ' . shellescape(a:filename))
  if v:shell_error !=# 0 || len(l:output) ==# 0
    throw 'Command error: ''file --mime-type'': ' . join(l:output, "\n")
  endif

  let l:file_output = l:output[0]
  let l:mimetype = substitute(l:file_output, '\v^.*\:\s*(.*)\s*$', '\1', '')
  if l:mimetype ==# '' || stridx(l:mimetype, '/') ==# -1
    return 'The MIME type could not be detected'
  endif

  return l:mimetype
endfunction

" }}}

" UNIX wrappers {{{

" Load SSH agent
function! unix#load_ssh_agent() abort
  let l:lines = systemlist('esa env')
  if len(l:lines) < 3 || l:lines[0] !=# 'esa'
    " echoerr 'Unable to read the ssh agent PID file ' . l:pid_file
    return {}
  endif

  let l:ssh_auth_sock = l:lines[1]
  let l:ssh_agent_pid = l:lines[2]

  let $SSH_AUTH_SOCK = l:ssh_auth_sock
  let $SSH_AGENT_PID = l:ssh_agent_pid

  return {'SSH_AGENT_PID': l:ssh_agent_pid, 'SSH_AUTH_SOCK': l:ssh_auth_sock}
endfunction

" Which
function! unix#full_path_command(command) abort
  if !executable('which')
    echomsg 'Command not found: which'
    return ''
  endif

  let l:output = systemlist('which ' . shellescape(a:command))
  if v:shell_error !=# 0
    return ''
  endif

  if len(l:output) > 0 && executable(l:output[0])
    return l:output[0]
  endif

  return ''
endfunction

function! unix#edit_command(command) abort
  let l:command_path = unix#full_path_command(a:command)
  if l:command_path ==# ''
    echomsg 'Command not found: ' . a:command
    return
  else
    execute 'edit ' . fnameescape(l:command_path)
    " echo 'Edit: ' . l:command_path
  endif
endfunction

" Find
function! unix#find(fnmatch) abort
  let l:find_cmd = ''
  let l:find_cmd .= 'find -L . '
  for l:dir_name in g:unix_find_ignore_dirs
    let l:find_cmd .= printf('-not \( -type d -a -name %s -prune \) ',
          \ shellescape(l:dir_name))
  endfor
  let l:find_cmd .= '-not \( -type d \) -name ' . shellescape(a:fnmatch)
  let l:find_cmd .= ' | xargs file | sed "s/:/:1:/"'

  call vim#cfile_cmd(l:find_cmd)
endfunction

" Delete a file
function! s:delete_file(file) abort
  if executable('trash-put')
    let l:output = system('trash-put ' . shellescape(a:file))
    if v:shell_error !=# 0
      echomsg 'trash-put: ' . l:output
      return -1
    endif
  else
    return delete(a:file)
  endif
  return 0
endfunction

" Check if a file can be renamed or modified
function! s:file_operations_checks(force) abort
  if &buftype !=# ''
    echoerr printf("The buftype '%s' is not supported", &buftype)
    return 1
  endif

  if !&modifiable
    echoerr printf('The buffer %s is not modifiable', bufnr(''))
    return 1
  endif

  if &modified
    if a:force
      write
      if &modified
        echoerr printf('Cannot save the buffer %s', bufnr(''))
        return 1
      endif
    else
      echoerr 'No write since last change for buffer ' .
            \ bufnr('') . ' (add ! to override)'
      return 1
    endif
  endif

  if expand('%:p') ==# ''
    echoerr printf('The name of the buffer %s is empty', bufnr(''))
    return 1
  endif

  return 0
endfunction

function! s:error_file_name(name) abort
  if match(a:name, '\v[\/\\]') !=# -1
      echoerr 'You must provide a file name, not a path'
      return 1
  endif
  return 0
endfunction

function! unix#reload(force) abort
  if s:file_operations_checks(a:force)
    return 1
  endif

  " Read buffer information
  let l:bufname = bufname()
  if empty(l:bufname)
    echoerr 'The buffer name is empty.'
    return 1
  endif

  if !filereadable(l:bufname)
    echoerr 'The file does not exist: ' . l:bufname
    return 1
  endif

  let l:bufnr = bufnr('')
  let l:windows = win_findbuf(l:bufnr)

  " Save view
  " let l:view = view#mkview(win_getid())

  " enew all windows
  for l:win_id in l:windows
    call win_execute(l:win_id, 'ViewSave')
    call win_execute(l:win_id, 'enew')
  endfor
  enew

  " wipeout
  execute 'bwipeout ' . l:bufnr

  " re-edit the file on all windows
  execute 'edit ' . fnameescape(l:bufname)
  for l:win_id in l:windows
    call win_execute(l:win_id, ':buffer ' . fnameescape(l:bufname))
  endfor

  for l:win_id in l:windows
    call win_execute(l:win_id, 'ViewLoad')
  endfor

  " Restore view
  " call view#loadview(l:view, win_getid(), 0)

  return 0
endfunction

"
" Remove a file and keep the file's windows open
"
function! unix#xdg_open(dir) abort
  let l:output = system('xdg-open ' . shellescape(a:dir))
  if v:shell_error !=# 0
    echomsg 'Error: xdg-open: ' . l:output
    return
  endif
endfunction

"
" Remove a file and keep the file's windows open
"
function! unix#remove(force) abort
  if s:file_operations_checks(a:force)
    return 1
  endif

  let l:file = expand('%:p')

  call buffer#enew(bufnr(''))

  let l:cwd = getcwd()
  silent execute 'bwipeout! ' . fnameescape(l:file)

  if bufloaded(l:file) || (filereadable(l:file) && s:delete_file(l:file))
    echoerr printf('Failed to delete "%s"', l:file)
    return 1
  endif

  execute 'lcd ' . fnameescape(l:cwd)
endfunction

"
" Delete the file but not the buffer
"
function! unix#unlink(force) abort
  if s:file_operations_checks(a:force)
    return 1
  endif

  let l:file = expand('%:p')
  if filereadable(l:file) && s:delete_file(l:file)
    echoerr printf('Failed to delete "%s"', l:file)
    return 1
  endif
endfunction

"
" Delete a file and its buffer
"
function! unix#delete(force) abort
  if s:file_operations_checks(a:force)
    return 1
  endif

  let l:file = expand('%:p')
  silent execute 'bwipeout! ' . fnameescape(l:file)
  if bufloaded(l:file) || (filereadable(l:file) && s:delete_file(l:file))
    echoerr 'Failed to delete "' . l:file . '"'
  endif
endfunction

"
" Smart rename
"
function! unix#smartmv(name_or_path, force, is_path) abort
  if s:file_operations_checks(a:force)
    return 1
  endif

  let l:mv_bin = 'smartmv'
  if !executable(l:mv_bin)
    return unix#rename(a:name_or_path, a:force)
  endif

  let l:buf_windows = win_findbuf(bufnr(''))
  let l:oldfile = expand('%:p')
  let l:newfile = s:smart_mv_or_cp_path(a:name_or_path, a:is_path)

  " if s:error_file_name(a:name_or_path)
  "   return 1
  " endif

  silent execute 'file' . ((a:force) ? '!' : '') . ' ' . fnameescape(l:newfile)

  try
    let l:output = system(
          \ printf(
          \   '%s %s %s',
          \   shellescape(l:mv_bin),
          \   shellescape(l:oldfile),
          \   shellescape(l:newfile))
          \ )
    if v:shell_error !=# 0
      echomsg 'smartmv: ' . l:output
    endif
  catch
    echoerr string(v:exception)
  finally
    if filereadable(l:oldfile)
      call s:delete_file(l:oldfile)
    endif
  endtry

  for l:window in l:buf_windows
    call win_execute(l:window, 'write!')
  endfor

  let l:force = 0
  call unix#reload(l:force)

  return 0
endfunction

"
" Rename
"
function! unix#rename(name, force) abort
  if s:file_operations_checks(a:force)
    return 1
  endif

  let l:oldfile = expand('%:p')
  let l:newfile = expand('%:h') . path#sep() . a:name
  let l:bufnr = bufnr('')

  " if s:error_file_name(a:name)
  "   return 1
  " endif

  execute 'saveas' . ((a:force) ? '!' : '') . ' ' . fnameescape(l:newfile)
  execute 'bwipeout! ' . fnameescape(l:oldfile)

  if bufloaded(l:oldfile) || (filereadable(l:oldfile) && s:delete_file(l:oldfile))
    echoerr 'Failed to delete "' . l:oldfile . '"'
  endif

  let l:force = 0
  call unix#reload(l:force)

  return 0
endfunction


"
" Copy
"
function! unix#copy(name_or_path, force, ...) abort
  let l:change_buffer = (len(a:000) > 0 && a:000[0]) ? 1 : 0

  if s:file_operations_checks(a:force)
    return 1
  endif

  let l:oldfile = expand('%:p')
  let l:newfile = s:smart_mv_or_cp_path(a:name_or_path, 1)
  let l:bufnr = bufnr('')

  " if s:error_file_name(a:name)
  "   return 1
  " endif

  if l:change_buffer
    silent execute 'saveas' . ((a:force) ? '!' : '') . ' ' . fnameescape(l:newfile)
    silent execute 'write' . ((a:force) ? '!' : '') . ' ' . fnameescape(l:oldfile)
  else
    silent execute 'write' . ((a:force) ? '!' : '') . ' ' . fnameescape(l:newfile)
  endif

  return 0
endfunction

"
" Chmod
"
function! unix#chmod(perm, force) abort
  if s:file_operations_checks(a:force)
    return 1
  endif

  let l:file = expand('%:p')

  let l:chmod_bin = 'chmod'
  if !executable(l:chmod_bin)
    throw 'No ''' . l:chmod_bin . ''' command in ' . $PATH
  endif

  let l:cmd = printf(
        \ '%s %s %s',
        \ shellescape(l:chmod_bin),
        \ shellescape(a:perm),
        \ shellescape(l:file)
        \ )
  let l:output = system(l:cmd)
  if v:shell_error !=# 0
    echomsg 'Error: ' . l:cmd . ': ' . l:output
    return 1
  endif

  return 0
endfunction

"
" Mkdir
"
function! unix#mkdir(dir) abort
  call mkdir(a:dir, 'p', 0755)
endfunction

""" }}}
