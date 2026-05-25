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
" THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
" SOFTWARE.

" netrw_switch.vim
" Requires: view.vim

function! s:file_explorer_get_config() abort
  return {'filetype': get(g:, 'fileexplorerswitch_filetype', 'netrw'),
      \   'cmd': get(g:, 'fileexplorerswitch_cmd', 'edit .')}
endfunction

function! s:file_explorer_save() abort
  let l:config = s:file_explorer_get_config()

  let l:fileexplorerswitch_data = {
        \ 'mkview': view#mkview(win_getid()),
        \ 'bufnr': bufnr(''),
        \ 'buftype': &buftype,
        \ 'winsaveview': winsaveview(),
        \ 'cwd': getcwd()
        \ }

  if &filetype ==# l:config['filetype']
    let w:fileexplorer_data_fe = l:fileexplorerswitch_data
  else
    let w:fileexplorer_data_normal = l:fileexplorerswitch_data
  endif
endfunction

function! file_explorer_switch#switch() abort
  let l:config = s:file_explorer_get_config()

  let l:restore = 0

  if &filetype ==# l:config['filetype']
    " Switch to normal
    if exists('w:fileexplorer_data_normal')
      let l:restore = 1
      let l:restore_data_type = 'normal'
      let l:restore_data = w:fileexplorer_data_normal
    endif
  else
    " Switch to file explorer
    if exists('w:fileexplorer_data_fe')
      let l:restore = 1
      let l:restore_data_type = 'file_explorer'
      let l:restore_data = w:fileexplorer_data_fe
    else
      execute l:config['cmd']
    endif
  endif

  if l:restore
    execute 'buffer ' . string(l:restore_data['bufnr'])
    if l:restore_data['buftype'] ==# ''
      call view#loadview(l:restore_data['mkview'], win_getid(), 1)
    else
      call winrestview(l:restore_data['winsaveview'])
    endif

    if l:restore_data_type ==# 'normal' && l:restore_data['cwd'] !=# getcwd()
      echo 'chdir: ' . l:restore_data['cwd']
      call chdir(l:restore_data['cwd'])
    endif
  endif
endfunction

function! s:file_explorer_open() abort
  let l:config = s:file_explorer_get_config()
  execute l:config['cmd']
endfunction

function! file_explorer_switch#init() abort
  augroup FileExplorerSwitch
    autocmd!
    autocmd BufLeave * call s:file_explorer_save()
  augroup END

  nnoremap <silent> ,fe :call file_explorer_switch#switch()<CR>
  nnoremap <silent> ,fE :call <SID>file_explorer_open()<CR>
endfunction

" Specific to: netrw {{{
function! file_explorer_switch#netrw_switch() abort
  let l:restore = 0

  if &filetype ==# 'netrw'
    " Switch to normal
    if exists('w:file_explorer_data_normal')
      let l:restore = 1
      let l:restore_data_type = 'normal'
      let l:restore_data = w:file_explorer_data_normal
    endif
  else
    " Switch to Netrw
    if exists('w:file_explorer_data_netrw')
      if w:file_explorer_data_netrw['netrw_liststyle'] < 3
        let l:reopen_netrw = 0
        let l:netrw_curdir = fnameescape(w:file_explorer_data_netrw['netrw_curdir'])

        let l:restore = 1
        let l:restore_data_type = 'netrw'
        let l:restore_data = w:file_explorer_data_netrw
      else
        let l:reopen_netrw = 1
        let l:netrw_restview = 1
        let l:netrw_curdir = fnameescape(w:file_explorer_data_netrw['netrw_curdir'])
      endif
    else
      let l:reopen_netrw = 1
      let l:netrw_restview = 0
      let l:netrw_curdir = '.'
    endif

    if l:reopen_netrw
      " execute 'silent edit ' . fnameescape(l:netrw_curdir)
      " call netrw#Explore(0, 0, 0, l:netrw_curdir)
      execute 'silent Explore ' . fnameescape(l:netrw_curdir)
      " if exists('w:file_explorer_data_netrw')
      "   execute 'silent Explore ' .
      "         \ fnameescape(w:file_explorer_data_netrw.cwd)
      " endif
      " let w:file_explorer_netrw_bufnr = bufnr('')
    endif

    if l:netrw_restview
      call winrestview(w:file_explorer_data_netrw['winsaveview'])
    endif
  endif

  if l:restore
    let l:win_id = win_getid()
    let l:edit_file = 1

    if l:restore_data['buftype'] !=# ''
      execute 'buffer ' . l:restore_data['bufnr']
    else
      call view#loadview(l:restore_data['view'], l:win_id, l:edit_file)
      if l:restore_data_type ==# 'normal' && exists('w:file_explorer_data_netrw') && w:file_explorer_data_netrw['cwd'] !=# getcwd()
        echo 'chdir: ' . w:file_explorer_data_netrw['cwd']
        call chdir(w:file_explorer_data_netrw['cwd'])
      endif
    endif
  endif
endfunction

function! file_explorer_switch#netrw_file_explorer_save() abort
  let l:file_explorer_data = {
        \ 'view': view#mkview(win_getid()),
        \ 'bufnr': bufnr(''),
        \ 'buftype': &buftype,
        \ 'winsaveview': winsaveview(),
        \ 'cwd': getcwd()
        \ }

  if &filetype ==# 'netrw'
    let l:netrw_liststyle = get(w:, 'netrw_liststyle', get(g:, 'netrw_liststyle', 0))
    let l:file_explorer_data['netrw_liststyle'] = l:netrw_liststyle
    if exists('b:netrw_curdir')
      let l:file_explorer_data['netrw_curdir'] = b:netrw_curdir
    else
      let l:file_explorer_data['netrw_curdir'] = '.'
    endif

    let w:file_explorer_data_netrw = l:file_explorer_data
  else
    let w:file_explorer_data_normal = l:file_explorer_data
  endif
endfunction

function! file_explorer_switch#netrw_hidden_files_init() abort
  let l:escape = 'substitute(escape(v:val, ".$~"), "*", ".*", "g")'
  let l:dotfiles = '\(^\|\s\s\)\zs\.\S\+'
  if !exists('g:netrw_list_hide_wildignore')
    let g:netrw_list_hide_wildignore =
          \ join(map(split(&wildignore, ','), '"^".' . l:escape . '. "/\\=$"'), ',')
  endif

  if !exists('g:netrw_list_hide_wildignore_and_hidden')
    let g:netrw_list_hide_wildignore_and_hidden = g:netrw_list_hide_wildignore . ',' . l:dotfiles
  endif

  if !exists('g:netrw_list_hide')
    let g:netrw_list_hide = g:netrw_list_hide_wildignore_and_hidden
  endif
endfunction

function! file_explorer_switch#netrw_switch_hidden_files() abort
  if &filetype ==# 'netrw'
    if g:netrw_list_hide ==# g:netrw_list_hide_wildignore_and_hidden
      let g:netrw_list_hide = g:netrw_list_hide_wildignore
    else
      let g:netrw_list_hide = g:netrw_list_hide_wildignore_and_hidden
    endif

    if exists('b:netrw_curdir')
      execute ':Explore ' . fnameescape(b:netrw_curdir)
    endif
  endif
endfunction

" }}}
