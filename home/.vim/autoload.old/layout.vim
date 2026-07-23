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

" layout.vim
" Save / load the layout
" Requires: view.vim

function! s:layout_get_win_info(win_id) abort
  let l:bufnr = winbufnr(a:win_id)
  let l:active = (win_getid() ==# a:win_id)

  let l:view = []
  let l:bufname = ''
  if &buftype ==# ''
    let l:bufname = bufname(l:bufnr)

    if filereadable(l:bufname) || isdirectory(l:bufname)
      let l:bufname = fnamemodify(l:bufname, ':p')
    else
      let l:bufname = ''
    endif

    " Save view
    let l:view = view#mkview(a:win_id)
  endif

  return {'bufname': l:bufname,
  \       'winwidth': winwidth(a:win_id),
  \       'winheight': winheight(a:win_id),
  \       'columns': &columns,
  \       'lines': &lines,
  \       'winsaveview': winsaveview(),
  \       'curpos': getcurpos(a:win_id),
  \       'active': l:active,
  \       'view': l:view,
  \       'cwd': getcwd(a:win_id)}
endfunction

function! s:layout_set_win_info(win_info) abort
  " Set window info
  if a:win_info['bufname'] !=# ''
    if bufexists(a:win_info['bufname'])
      execute 'silent buffer ' . a:win_info['bufname']
    else
      execute 'silent edit ' . fnameescape(a:win_info['bufname'])
    endif
  endif
  execute 'lcd ' . fnameescape(a:win_info['cwd'])
  " call setpos('.', a:win_info['curpos'])
  " call winrestview(a:win_info['winsaveview'])

  " Calculate width and height
  let l:width = a:win_info['winwidth']
  let l:height = a:win_info['winheight']

  if &columns !=# a:win_info['columns']
    let l:width = str2nr((&columns * l:width) / a:win_info['columns'])
  endif

  if &lines !=# a:win_info['lines']
    let l:height = str2nr((&lines * l:height) / a:win_info['lines'])
  endif

  " Load view
  " call view#loadview(a:win_info['view'])

  " Return the width and height (the resize will be done later)
  return {'winnr': winnr(),
  \       'win_id': win_getid(),
  \       'width': l:width,
  \       'height': l:height,
  \       'win_info': a:win_info}
endfunction

function! s:layout_get_state(...) abort
  let l:layout = (len(a:000) ==# 0) ? winlayout() : a:000[0]

  if l:layout[0] ==# 'leaf'
    let l:layout[1] = s:layout_get_win_info(l:layout[1])
  else
    for l:layout_child in l:layout[1]
      call s:layout_get_state(l:layout_child)
    endfor
  endif

  return l:layout
endfunction

function! s:layout_set_state(layout) abort
  let l:result = []
  let l:split_type = a:layout[0]

  if l:split_type ==# 'leaf'
    let l:window_size = s:layout_set_win_info(a:layout[1])
    call add(l:result, l:window_size)
    return l:result
  endif

  let l:first = 1
  for l:layout_child in a:layout[1]
    if l:first
      let l:first = 0
    else
      if l:split_type ==# 'col'
        rightbelow split
      elseif l:split_type ==# 'row'
        rightbelow vsplit
      else
        throw '"' . a:layout[0] . '" is an invalid value (only "row", "col" and "leaf" are supported)'
      endif
    endif

    " Recursive
    let l:list_window_size = s:layout_set_state(l:layout_child)

    " Add the window sizes l:result
    for l:window_size in l:list_window_size
      call add(l:result, l:window_size)
    endfor
  endfor

  return l:result
endfunction

function! layout#save() abort
  return {'layout_state': s:layout_get_state(), 'tabpagenr': tabpagenr()}
endfunction

function! layout#load(layout_save) abort
  silent only
  enew
  let l:list_win_size = s:layout_set_state(a:layout_save['layout_state'])

  let l:active_win_id = -1
  for l:win_size in l:list_win_size
    if l:win_size['win_info']['active']
      let l:active_win_id = l:win_size['win_id']
    endif

    let l:edit_file = 0
    silent call view#loadview(l:win_size['win_info']['view'], l:win_size['win_id'], l:edit_file)
    " call winrestview(l:win_size['win_info']['winsaveview'])

    let l:resize_cmd = l:win_size['winnr'] . 'resize '
    execute 'vertical ' . l:resize_cmd . ' ' . l:win_size['width']
    execute  l:resize_cmd . ' ' . l:win_size['height']
  endfor

  if l:active_win_id !=# -1
    call win_gotoid(l:active_win_id)
  endif
endfunction
