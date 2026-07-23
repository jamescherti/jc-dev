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

" vim.vim

" MOVED TO MY VIMRC
" function! vim#select_pasted_text() range
"   call execute('normal! `[' . strpart(getregtype(), 0, 1) . '`]')
"   if &selection ==# 'exclusive'
"     normal! l
"   endif
" endfunction

function! vim#complete_command_sibling_files(A, L, P) abort
  if stridx(a:A, '/') !=# -1 || stridx(a:A, '\') !=# -1
    "
    " It is a path (absolute or relative)
    "
    if a:A[0:len('/')-1] ==# '/'
      let l:path = fnamemodify(fnamemodify(a:A, ':h'), ':p')
    else
      let l:path = fnamemodify(path#rstrip_sep(expand('%:h')) . path#sep() . fnamemodify(a:A, ':h'), ':p')
    endif

    let l:pattern = fnamemodify(a:A, ':t') . '*'
    let l:func = 'v:val'
  else
    "
    " Just a filename
    "
    let l:path = expand('%:h') . path#sep()
    let l:pattern = a:A . '*'
    let l:func = 'fnamemodify(v:val, '':t'')'
  endif

  let l:list_paths = map(globpath(l:path, l:pattern, 0, 1), '(isdirectory(v:val)) ? v:val . path#sep() : v:val')
  return map(l:list_paths, l:func)
endfunction

" call vim#map_command('niv', ',el', 'NameFunction("arg1")')
function! vim#map_command(modes, key_mapping, ...) abort
  let l:has_cmd = 0
  let l:pre_cmd = ':'
  let l:post_cmd = ":echom ''<CR>"
  if has('patch-8.2.1978')
    " The 'patch-8.2.1978' adds the '<cmd>' feature
    let l:has_cmd = 1
    let l:pre_cmd = '<cmd>'
    let l:post_cmd = ''
  endif

  for l:mode in split(a:modes, '\zs')
    let l:local_pre_cmd = l:pre_cmd
    if l:has_cmd ==# 0
      if l:mode ==# 't'
        let l:local_pre_cmd = '<C-w>' . l:local_pre_cmd
      elseif l:mode !=# 'n'
        let l:local_pre_cmd = '<Esc>' . l:local_pre_cmd
      endif
    endif

    let l:local_post_cmd = l:post_cmd
    if l:has_cmd ==# 0
      if l:mode ==# 'v'
        let l:local_post_cmd .= 'gv'
      endif

      if l:mode ==# 'i'
        let l:local_post_cmd .= 'a'
      endif
    endif

    let l:local_command = ''
    for l:item_command in a:000
      let l:local_command .= l:local_pre_cmd .
        \ ((l:has_cmd ==# 0 && l:mode ==# 'v') ? '<C-U>' : '') .
        \ l:item_command . '<CR>'
    endfor

    execute l:mode . 'noremap ' . a:key_mapping . ' ' .
      \ l:local_command . l:local_post_cmd
  endfor
endfunction

" Remplace all map all
function! vim#map_all(map, command) abort
  execute 'nnoremap <silent> ' . a:map . ' ' . a:command . '<CR>'
  if stridx(a:map, '<Leader') ==# -1
    " if stridx(tolower(a:command), "<cmd>") >= 0
    "   execute 'vnoremap ' . a:map . ' ' . a:command . '<CR>'
    "   execute 'inoremap ' . a:map . ' ' . a:command . '<CR>'
    " else
    execute 'vnoremap <silent> ' . a:map . ' <Esc>' . a:command . '<CR>gv'
    execute 'inoremap <silent> ' . a:map . ' <Esc>' . a:command . '<CR>a'
    " endif
  endif
endfunction

function! vim#save_all() abort
  let s:confirm = &confirm
  set noconfirm
  try
    silent! wall
  catch
  finally
    let &confirm = s:confirm
  endtry
endfunction

function! vim#migrate_dir(old_path, new_path) abort
  if isdirectory(a:old_path) || filereadable(a:old_path)
    if ! isdirectory(a:new_path) && ! filereadable(a:new_path)
      call rename(a:old_path, a:new_path)
    endif
  else
    if isdirectory(a:old_path)
      call mkdir(a:new_path, 'p', 0700)
    endif
  endif
endfunction

function! vim#disable_mouse() abort
  let l:list_mouse_click = [
        \ 'LeftMouse', 'LeftDrag', 'LeftRelease', 'MiddleMouse',
        \ 'MiddleDrag', 'MiddleRelease', 'RightMouse',
        \ 'RightDrag', 'RightRelease', 'X1Mouse', 'X1Drag',
        \ 'X1Release', 'X2Mouse', 'X2Drag', 'X2Release']

  for l:mode in ['n', 'i', 'v', 'c']
    execute printf('%snoremap <MiddleDrag> <Nop>', l:mode)

    " Mouse clicks
    for l:button in l:list_mouse_click
      execute printf('%snoremap <%s> <Nop>', l:mode, l:button)
      execute printf('%snoremap <%s> <Nop>', l:mode, l:button)
      execute printf('%snoremap <%s> <Nop>', l:mode, l:button)
    endfor

    " 2-, 3-... are double clicks
    for l:modifier in ['', 'C-', 'S-', 'A-']
      for l:clicks in ['2-', '3-', '4-']
        for l:button in ['RightMouse', 'LeftMouse', 'MiddleMouse']
          execute printf('%snoremap <%s%s%s> <Nop>', l:mode, l:modifier, l:clicks, l:button)
          execute printf('%snoremap <%s%s%s> <Nop>', l:mode, l:modifier, l:clicks, l:button)
          execute printf('%snoremap <%s%s%s> <Nop>', l:mode, l:modifier, l:clicks, l:button)
        endfor
      endfor

      execute printf('%snoremap <%sScrollWheelUp> <nop>', l:mode, l:modifier)
      execute printf('%snoremap <%sScrollWheelDown> <nop>', l:mode, l:modifier)
      execute printf('%snoremap <%sScrollWheelLeft> <nop>', l:mode, l:modifier)
      execute printf('%snoremap <%sScrollWheelRight> <nop>', l:mode, l:modifier)
    endfor
  endfor
endfunction

function! vim#quit() abort
  call terminal#close_all()
  call vim#save_all()
  call buffer#close_all_encrypted()

  let l:buflist = getbufinfo()
  for l:buf in l:buflist
    if getbufvar(l:buf.bufnr, '&modified', 0)
       echoerr 'No write since last change for buffer ' . l:buf['bufnr'] .
          \ ' (add ! to override)'
       return
    endif
  endfor

  execute ':wqall'
endfunction

function! vim#reset_key_maps() abort
  mapclear | mapclear <buffer> | mapclear! | mapclear! <buffer>
endfunction

function! vim#cmd_alias(cmd, ...) abort
  for l:alias in a:000
    let l:alias = substitute(escape(l:alias, '|'), "'", "''", 'g')
    execute printf(
          \ 'cnoreabbrev <expr> %s ' .
          \ 'getcmdtype() == '':'' && ' .
          \ 'getcmdline() == ''%s'' ? ''%s'' : ''%s''',
          \ l:alias,
          \ l:alias,
          \ substitute(escape(a:cmd, '|'), "'", "''", 'g'),
          \ l:alias,
          \ )
  endfor
endfunction

function! vim#viminfo_path() abort
  if &viminfofile
    return fnamemodify(&viminfofile, ':p')
  endif

  for l:item in split(&viminfo, ',')
    if len(l:item) > 0 && l:item[0] ==# 'n'
      return fnamemodify(l:item[1:], ':p')
    endif
  endfor

  " :help viminfo-file-name
  if has('win32') && !has('win32unix')
    if exists('$HOME')
      return fnamemodify($HOME . '\_viminfo', ':p')
    elseif exists('$VIM')
      return fnamemodify($VIM . '\_viminfo', ':p')
    else
      return 'c:\_viminfo'
    endif
  else
    return fnamemodify('~/.viminfo', ':p')
  endif
endfunction

function! vim#viminfo_watchdog() abort
  " File size
  let l:viminfo_path = fnamemodify(vim#viminfo_path(), ':p')
  if !filereadable(l:viminfo_path)
    return
  endif
  let l:size_mb = getfsize(l:viminfo_path) / 1024 / 1024
  if l:size_mb > 1
    echoerr printf('The viminfo file is too big: %s', l:viminfo_path)
  endif

  " Tmp files
  let l:viminfo_dir = fnamemodify(l:viminfo_path, ':p:h') . '/*.tmp'
  let l:tmpfiles = split(glob(l:viminfo_dir), "\n")
  if len(l:tmpfiles) > 1
    echoerr 'More than one Viminfo tmpfiles exist: ' . string(l:tmpfiles)
  endif
endfunction

function! vim#cfile_list(list) abort
  let l:error_file = tempname()
  try
    call writefile(a:list, l:error_file)
    setlocal errorformat=%f
    execute 'cfile ' . fnameescape(l:error_file)
  finally
    call delete(l:error_file)
  endtry
endfunction

function! vim#cfile_cmd(cmd) abort
  let l:error_file = tempname()
  let l:cmd = a:cmd
  let l:cmd .= ' > '. shellescape(l:error_file)
  try
    call system(l:cmd)
    " call execute(':!' . l:cmd)
    if v:shell_error ==# 0
      setlocal errorformat=%f:%l:%m
      execute 'cfile ' . fnameescape(l:error_file)
    endif
  finally
    call delete(l:error_file)
  endtry
endfunction

"
" Return the directory where one of the files or directories names in
" the list 'a:list_file_names' exists.
"
" Return an empty string if none of the files in a:list_file_names exist in
" the 'a:dir' or in its parent directories.
"
" Example: vim#find_in_current_and_parent_dirs('.', ['.git/', 'setup.py'])
"
function! vim#find_in_current_and_parent_dirs(dir, list_file_names) abort
  let l:result = []

  let l:cur_dir = path#rstrip_sep(fnamemodify(a:dir, ':p'))
  if !isdirectory(l:cur_dir)
    return l:result
  endif

  while 1
    " Check if one of the files/directories in a:list_file_names exist
    let l:one_of_paths_exist = path#one_of_files_exist(l:cur_dir, a:list_file_names)
    if !empty(l:one_of_paths_exist)
      call add(l:result, l:one_of_paths_exist)
    endif

    " Previous directory
    let l:prev_curdir = l:cur_dir
    let l:cur_dir = fnamemodify(l:cur_dir, ':h')
    if l:cur_dir ==# l:prev_curdir
      break
    endif
  endwhile

  return result
endfunction

function! vim#chdir_project_dir() abort
  let l:list_project_dir = vim#find_in_current_and_parent_dirs('..', [
        \ '.git/',
        \ 'autoload/', 'ftdetect/', 'ftplugin/', 'plugin/', 'syntax/', 'colors/', 'after/',
        \ 'setup.py', 'setup.cfg',
        \ 'install.sh'])
  if len(l:list_project_dir) <= 0
    return
  endif

  let l:project_dir = l:list_project_dir[0]
  execute 'lcd ' .  fnamemodify(l:project_dir, ':p:h')
  execute 'lcd ..'
  pwd
endfunction

function! vim#find_pre(filename) abort
  if &modified
    echoerr 'No write since last change.'
    return
  endif

  let l:list_files = vim#find_in_current_and_parent_dirs('.', [a:filename])

  if len(l:list_files) ==# 0
    echo 'Files not found.'
    return
  endif

  call vim#cfile_list(l:list_files)
endfunction
