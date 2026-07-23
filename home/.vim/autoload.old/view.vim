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

" view.vim

function! s:go_to_window_id(win_id) abort
  let l:prev_win_id = win_getid()
  if l:prev_win_id ==# a:win_id
    return -1
  endif

  if ! win_gotoid(a:win_id)
    throw 'Unable to go to the window id: ' . a:win_id
  endif

  return l:prev_win_id
endfunction

function! s:restore_window_id(prev_win_id) abort
  if a:prev_win_id ==# -1
    return
  endif

  if a:prev_win_id ==# win_getid()
    return
  endif

  if ! win_gotoid(a:prev_win_id)
    throw 'Unable to restore the window id: ' . a:prev_win_id
  endif
endfunction

function! view#mkview(win_id)
  try
    let l:prev_win_id = s:go_to_window_id(a:win_id)
  catch
    echoerr v:exception
    return
  endtry

  let l:tempfile = tempname()
  let l:save_viewoptions = &viewoptions
  let l:result = {}
  try
    set viewoptions=folds,cursor,curdir,slash,unix

    if &buftype ==# ''
      silent execute 'mkview ' . fnameescape(l:tempfile)
      let l:result['mkview'] = readfile(l:tempfile)
    else
      let l:result['mkview'] = []
    endif

    let l:result['bufnr'] = bufnr('')
    let l:result['bufname'] = bufname(l:result['bufnr'])
    let l:result['buftype'] = &buftype
  catch
    echoerr 'view#mkview(): ' . v:exception
  finally
    let &viewoptions = l:save_viewoptions

    try
      call s:restore_window_id(l:prev_win_id)
    finally
      call delete(l:tempfile)
    endtry
  endtry

  return l:result
endfunction

function! view#loadview(view, win_id, edit_file)
  try
    let l:prev_win_id = s:go_to_window_id(a:win_id)
  catch
    echoerr v:exception
    return
  endtry

  try
    for l:line in a:view['mkview']
      " Ignore: if bufexists(".vimrc") | buffer .vimrc | else | edit .vimrc | endif
      if !a:edit_file && l:line =~# ' edit '
        " if bufexists(a:view['bufname'])
        "   execute 'silent buffer ' . a:view['bufnr']
        " else
        "   execute 'silent edit ' . a:view['bufname']
        " endif
        continue
      endif

      if l:line =~# 'doautoall'
        continue
      endif

      if l:line =~# 'argglobal'
        continue
      endif

      " echomsg l:line
      silent execute l:line
    endfor
  catch
    echo string(v:exception)
  finally
    call s:restore_window_id(l:prev_win_id)
  endtry
endfunction
