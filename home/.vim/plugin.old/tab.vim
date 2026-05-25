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

" tab.vim

if exists('g:loaded_tab_plugin') || &compatible
  finish
endif
let g:loaded_tab_plugin = 1

"
" Remember closed tabs
"
let g:remember_tab_stack = []
let g:remember_tab_max = 30

command! -nargs=0 TabClose call tab#close()
command! -nargs=0 TabReopen call tab#reopen()
command! -nargs=0 TabDuplicate call tab#duplicate()
command! -bar -nargs=1 TabMoveWrap call tab#move_wrap(<f-args>)

"
" Better tab line
"
if exists('+showtabline')
  command! -nargs=1 TabRename call tab#rename(<f-args>)

  "" let g:tab_label_function = 'MyTabLabelClassic'
  "let g:tab_label_function = 'MyTabLabelGrouped'

  "set tabline=%!tab#tabline()
endif

"function! s:set_guitabline() abort
"  if exists('+guitablabel')
"    set guitablabel=%{tab#guitabline()}
"    " set guioptions+=e
"  endif
"endfunction

" nnoremap ,tr :TabRename<space>

" augroup GuiTabLine
"   autocmd!
"   autocmd GUIEnter * call <SID>set_guitabline()
" augroup END

" Key mappings

nnoremap <C-g> <C-t>
" call vim#map_command('n', '<C-w>c', 'TabClose')
" call vim#map_command('n', '<C-w><C-c>', 'TabClose')
tnoremap <silent> <C-w>c <C-\><C-n><C-w>:bwipeout!<CR>
""call vim#map_command('t', '<C-t>', 'tabnew')
" call vim#map_command('n', '<C-t>', 'tab split')
" call vim#map_command('nt', '<C-S-w>', ':TabReopen')
" call vim#map_command('nt', '<C-S-t>', ':TabReopen')
" nnoremap <C-z> <C-w>
" vnoremap <C-z> <C-w>

" tnoremap <silent> <C-z> <C-\><C-n><C-w>:bwipeout!<CR>
" call vim#map_command('niv', '<C-g>', 'tab split')
" call vim#map_command('t', '<C-g>', 'tabnew')
" call vim#map_command('niv', '<C-z>', 'TabClose')
" call vim#map_command('nivt', '<C-S-z>', ':TabReopen')

" call vim#map_command('nivt', '<A-S-g>', ':tabnew')

function! TabMoveWrapRedraw(number) abort
  execute 'TabMoveWrap ' . a:number
  redraw!
endfunction
" call vim#map_command('t', '<C-S-k>', 'call TabMoveWrapRedraw(-1)')
call vim#map_command('niv', '<C-S-k>', 'TabMoveWrap -1')
call vim#map_command('niv', '<C-S-j>', 'TabMoveWrap 1')
call vim#map_command('t', '<C-S-k>', 'call TabMoveWrapRedraw(-1)')
call vim#map_command('t', '<C-S-j>', 'call TabMoveWrapRedraw(1)')

" tnoremap <silent> <C-S-k> :call <SID>tab_move_wrap_redraw(-1)<CR>
" tnoremap <silent> <C-S-j> :call <SID>tab_move_wrap_redraw(1)<CR>

tnoremap <silent> <A-S-g> <C-w>:tabnew<CR>
tnoremap <silent> <C-j> <C-w>gt
tnoremap <silent> <C-k> <C-w>gT
