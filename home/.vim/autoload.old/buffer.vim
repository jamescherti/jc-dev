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

" File: buffer.vim

" Return true if successful
function! buffer#goto_bufname(bufname) abort
  let l:bufname = fnamemodify(a:bufname, ':p')
  if !bufexists(l:bufname)
    return 0  " Failure
  endif

  let l:bufnr = bufnr(l:bufname)
  if bufnr('') ==# l:bufnr
    return 1  " Success
  endif

  for l:win_id in win_findbuf(l:bufnr)
    if win_gotoid(l:win_id)
      return 1  " Success
    endif
  endfor

  return 0  " Failure
endfunction

" Go to the window where the buffer is if it is already loaded.
" Edit the file if the buffer is not loaded yet.
function! buffer#goto_bufname_or_edit(bufname, force, ...) abort
  let l:relative_to_current_dir = 0
  if len(a:000) > 0
    let l:relative_to_current_dir = a:1
  endif

  if l:relative_to_current_dir && a:bufname[0:len('/')-1] !=# '/'
    let l:bufname =
      \ fnamemodify(path#rstrip_sep(expand('%:h')) .
      \ path#sep() . a:bufname, ':p')
  else
    let l:bufname = fnamemodify(a:bufname, ':p')
  endif

  if isdirectory(l:bufname) || !buffer#goto_bufname(l:bufname)
    execute 'edit' . ((a:force) ? '!' : '') . ' ' . fnameescape(l:bufname)
  endif

  if bufexists(l:bufname)
    return 1  "  Success
  endif

  return 0  "  Failure
endfunction

function! buffer#enew(bufnr) abort
  for l:win_id in win_findbuf(a:bufnr)
    call win_execute(l:win_id, 'enew')
  endfor
endfunction

function! buffer#is_empty() abort
  if line('$') ==# 1 && getline(1) ==# ''
    return 1
  else
    return 0
  endif
endfunction

function! buffer#is_expendable() abort
  if !&modified && buffer#is_empty()
    return 1
  else
    return 0
  endif
endfunction

function! buffer#close_all_encrypted() abort
  let l:buflist = getbufinfo()
  for l:buf in l:buflist
    if getbufvar(l:buf.bufnr, '&key', '') !=# ''
      execute 'bwipeout! ' . l:buf.bufnr
    endif
  endfor
endfunction

function! buffer#change_tab(tab_size) abort
  let &l:tabstop = a:tab_size
  let &l:softtabstop = a:tab_size
  let &l:shiftwidth = a:tab_size
  setlocal expandtab
endfunction

function! buffer#special_chdir(dir) abort
  let l:oldcwd = getcwd()
  execute 'lcd ' . fnameescape(a:dir)
  if &filetype ==# 'nerdtree' && getcwd() != l:oldcwd
    edit .
  endif
  pwd
endfunction

" there is a bug with :grep
function! buffer#close_hidden() abort
  " \        getbufvar(v.bufnr, 'buftype') ==# '' &&
  let l:buffers = filter(
          \ getbufinfo(),
          \ {_,
          \  v -> (v.loaded &&
          \        len(v.windows) ==# 0 &&
          \        (!v.listed || v.hidden))
          \ })

  if !empty(l:buffers)
    execute 'bwipeout' join(map(l:buffers, {_, v -> v.bufnr}))
  endif
endfunction

function! s:no_modifiable_base() abort
  setlocal nomodified
  setlocal noswapfile
  setlocal nowrap
  setlocal nonumber
  setlocal nofoldenable
  setlocal foldcolumn=0
  if exists('&relativenumber')
    setlocal norelativenumber
  endif
  if exists('&signcolumn')
    setlocal signcolumn=no
  endif
endfunction

function! buffer#set_readonly() abort
  call s:no_modifiable_base()
  setlocal readonly
endfunction

function! buffer#set_nomodifiable() abort
  call buffer#set_readonly()
  setlocal nomodifiable
endfunction

function! buffer#set_nofile() abort
  setlocal buftype=nofile
  setlocal bufhidden=delete
endfunction

function! buffer#no_buf_listed() abort
  setlocal nobuflisted
endfunction

function! buffer#find_bufname(bufname) abort
  for l:buf in getbufinfo()
    if l:buf.hidden
      continue
    endif

    if bufname(l:buf.bufnr) == a:bufname
      return l:buf.bufnr
    endif
  endfor

  return -1
endfunction

function! buffer#switch_to_buffer(bufnr) abort
  let l:list_win_id = win_findbuf(a:bufnr)
  if len(l:list_win_id) > 0
    let l:bufnr = l:list_win_id[0]
    return win_gotoid(l:bufnr)  " true if successful
  endif
  return 0
endfunction

function! buffer#diff_with_saved() abort
  tab split
  let filetype=&filetype
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  execute 'setlocal bt=nofile bh=wipe nobl noswf ro'
  let &filetype = filetype
endfunction

function! buffer#diff_with_undo() abort
  call clearmatches()
  redir => message
  silent changes
  redir END
  let line = matchstr(message, '\v\n\s{4}1[^0-9]*\zs\d+\ze')
  highlight TemporalDiff ctermbg=green guibg=green
  let m = matchadd('TemporalDiff', '\%'.line.'l')
endfunction

function! buffer#auto_close_delimiter(char1, char2) abort
  return a:char1 . a:char2 . "\<left>"
endfunction

function! buffer#auto_close_all_delimiters() abort
  " Autoclose delimiter (like delimitmate)
  inoremap <buffer> <expr> " buffer#auto_close_delimiter('"', '"')
  inoremap <buffer> <expr> ' buffer#auto_close_delimiter("'", "'")
  inoremap <buffer> <expr> ` buffer#auto_close_delimiter("`", "`")
  inoremap <buffer> <expr> ( buffer#auto_close_delimiter('(', ')')
  inoremap <buffer> <expr> [ buffer#auto_close_delimiter('[', ']')
  inoremap <buffer> <expr> { buffer#auto_close_delimiter('{', '}')
  " inoremap <buffer> <S-Space> <Space><Space><Left>
endfunction

function! s:improved_auto_close_delimiter(char1, char2) abort
  let l:col = col('.') - 1
  let l:line = getline('.')

  " Character before and after
  if l:col == 0
    let l:char_before = ' '
  else
    let l:char_before = l:line[l:col - 1]
  endif

  if l:col >= len(l:line)
    let l:char_after = ' '
  else
    let l:char_after = l:line[l:col]
  endif

  return a:char1 . a:char2 . "\<left>"
endfunction

function! buffer#replace_word_under_cursor(...) abort
  let l:action = 'main'
  if len(a:000) > 0
    let l:action = a:000[0]
  endif

  "
  " Main action
  "
  if l:action ==# 'main'
    let b:replace_word_data = {}
    let b:replace_word_data['changenr'] = changenr()

    let l:regex_prefix = ''
    let l:regex_suffix = ''
    if mode() ==# 'n'
      "normal! diw
      normal! yiwel
      let l:regex_prefix = '\<'
      let l:regex_suffix = '\>'
    elseif mode() ==# 'V'
      normal! c
    elseif mode() ==# 'v'
      normal! d
    else
      echoerr 'Unsupported mode: ' . mode()
      return
    endif

    let b:replace_word_data['string'] = substitute(@@, '\v\n+$', '', 'g')
    let b:replace_word_data['escaped_string'] =
        \ '\V' . l:regex_prefix .
        \ escape(b:replace_word_data['string'], '/\') .
        \ l:regex_suffix
    startinsert

    augroup ReplaceString
      autocmd!
      autocmd InsertLeave *
        \ call buffer#replace_word_under_cursor('insert_leave')
    augroup END

    return
  endif

  "
  " Insert Leave
  "
  if l:action ==# 'insert_leave'
    autocmd! ReplaceString InsertLeave

    let l:new_string = expand('<cword>')
    if empty(l:new_string)
      execute 'undo ' . b:replace_word_data['changenr']
      return
    endif

    let l:cursor_pos = getpos('.')
    if b:replace_word_data['string'] !=# l:new_string
      execute 'keeppatterns silent! %substitute/' .
          \ b:replace_word_data['escaped_string'] . '/' .
          \ escape(l:new_string, '/\') . '/g'
    endif
    call setpos('.', l:cursor_pos)

    echo 'Replace: ' . b:replace_word_data['string'] . ' -> ' . l:new_string

    unlet b:replace_word_data
    return
  endif
endfunction
