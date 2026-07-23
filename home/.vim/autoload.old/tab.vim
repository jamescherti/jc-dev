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

"tab.vim

" Go to the tab where a buffer is open
function! tab#goto_bufname(bufname, ...) abort
  let l:tab_position = -1
  if len(a:000) ># 0
    let l:tab_position = a:000[0]
  endif

  let l:bufname = fnamemodify(a:bufname, ':p')
  if bufexists(l:bufname)
    if buffer#goto_bufname(l:bufname)
      return 1
    endif
  endif

  if !tab#is_last()
    tabnew
  endif

  if l:tab_position >=# 0
    execute 'tabmove ' . l:tab_position
  endif

  execute 'edit ' . fnameescape(l:bufname)
  if isdirectory(l:bufname)
    execute 'lcd ' . fnameescape(l:bufname)
  elseif !filereadable(l:bufname)
    echoerr 'The following directory does not exist: ' . l:bufname
  endif

  return 0
endfunction

function! tab#is_last() abort
  let l:is_last_tab = tabpagenr('$') <=# 1 &&
        \             winlayout()[0] ==# 'leaf' &&
        \             len(expand('%')) ==# 0 &&
        \             line('$') ==# 1 &&
        \             getline(1) ==# ''
  return l:is_last_tab
endfunction

" Remember tabs
function! tab#close() abort
  if tab#is_last()
    echo 'All tabs have been closed.'
    return
  endif

  let l:tab_previous = layout#save()

  try
    tabclose
  catch
    silent only
    enew
  endtry

  call add(g:remember_tab_stack, l:tab_previous)

  if g:remember_tab_max ==# 0
    let g:remember_tab_stack = []
  elseif g:remember_tab_max > 0 && len(g:remember_tab_stack) > g:remember_tab_max
    let g:remember_tab_stack = g:remember_tab_stack[-g:remember_tab_max:-1]
  endif
endfunction

function! tab#reopen() abort
  if len(g:remember_tab_stack) ==# 0
    echo 'All closed tabs have already been reopened.'
    return
  endif

  if ! tab#is_last()
    execute 'tabfirst'
    execute '-tabnew'
  endif

  call layout#load(g:remember_tab_stack[-1])
  execute 'tabmove ' . g:remember_tab_stack[-1]['tabpagenr']

  " Remove
  let g:remember_tab_stack = g:remember_tab_stack[0:-2]
endfunction

function! tab#duplicate() abort
  let l:layout = layout#save()
  tabnew
  call layout#load(l:layout)
endfunction

"
" Better Tab line
"
function! MyTabLabelClassic(tabnr) abort
  let l:bufnr = tabpagebuflist(a:tabnr)[tabpagewinnr(a:tabnr) - 1]

  let l:modified = 0
  if getbufvar(l:bufnr, '&modified')
    let l:modified = 1
  endif

  let l:tablabel = ''
  let l:custom_tablabel = gettabvar(a:tabnr, 'tablabel', '')
  if empty(l:custom_tablabel)
    let l:bufname = bufname(l:bufnr)
    if empty(l:bufname)
      let l:tablabel = empty(&buftype) ? 'No Name' : '<' . &buftype . '>'
    else
      let l:tablabel = fnamemodify(l:bufname, ':t')
    endif
  else
    let l:tablabel .= l:custom_tablabel
  endif

  if l:modified
    let l:tablabel .= '*'
  endif

  return l:tablabel
endfunction

function! MyTabLabelGrouped(tabnr) abort
  let l:tablabel = gettabvar(a:tabnr, 'tablabel', '')
  if ! empty(l:tablabel)
    return l:tablabel
  endif

  " Shows tab1|tab2|tab3 when the tab is not the current one and tab1 when it
  " is the current one
  " if a:tabnr ==# tabpagenr()
  "   let l:tabpagebuflist = [tabpagebuflist(a:tabnr)[tabpagewinnr(a:tabnr) - 1]]
  " else
  "   let l:tabpagebuflist = tabpagebuflist(a:tabnr)
  " endif

  " Shows tab1|tab2|tab3 all the time
  let l:tabpagebuflist = tabpagebuflist(a:tabnr)

  let l:tablabel = ''
  let l:bufnr_added = []
  for l:bufnr in l:tabpagebuflist
    if index(l:bufnr_added, l:bufnr) >= 0
      continue
    endif
    call add(l:bufnr_added, l:bufnr)

    " Tab label
    let l:bufname = path#rstrip_sep(bufname(l:bufnr))

    " Basename
    let l:new_tablabel = ''
    if !empty(l:bufname)
      if isdirectory(l:bufname)
        let l:new_tablabel .= 'dir:'
        let l:new_tablabel .=
          \ fnamemodify(fnamemodify(l:bufname, ':h'), ':t')
        let l:new_tablabel .= path#sep()
      endif

      let l:new_tablabel .= fnamemodify(l:bufname, ':t')
    endif

    " Add the label
    if l:tablabel !=# '' && l:new_tablabel !=# ''
      let l:tablabel .= '|'
    endif
    let l:tablabel .= l:new_tablabel

    if getbufvar(l:bufnr, '&modified')
      let l:tablabel .= '*'
    endif
  endfor

  if empty(l:tablabel)
    let l:tablabel = '-'
  endif

  return l:tablabel
endfunction

function! tab#rename(tablabel) abort
  let t:tablabel = a:tablabel
  execute 'redrawtabline'
endfunction

function! tab#edit(files) abort
  if len(a:files) > 0
    for l:filename in a:files
      echo l:filename
    endfor

    echo "\n"
    echo 'Directory: ' . getcwd()
    echo 'Number of files: ' . len(a:files)
    echo "\n"
    let l:answer = input('Edit? [y,n]')
    if l:answer !=# 'y'
      return
    endif
  endif

  " Edit the files in new tabs
  let l:first = 1
  for l:file in a:files
    if l:first
      " Edit the file in the current window
      let l:first = 0
    else
      " Edit the file in a new tab
      execute 'tabnew'
    endif

    execute 'edit ' . fnameescape(l:file)
  endfor
endfunction

function! tab#move_wrap(num_tabmoves) abort
  let l:lasttab = tabpagenr('$')
  let l:count_tabmoves = 0
  while l:count_tabmoves < abs(a:num_tabmoves)
    let l:curtab = tabpagenr()

    if a:num_tabmoves > 0
      if l:curtab >=# l:lasttab
        tabmove 0
      else
        tabmove +1
      endif
    else
      if l:curtab <=# 1
        tabmove $
      else
        tabmove -1
      endif
    endif

    let l:count_tabmoves += 1
  endwhile
endfunction
