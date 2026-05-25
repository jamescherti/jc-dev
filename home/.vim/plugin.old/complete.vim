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

" complete.vim

if exists('g:loaded_complete_plugin') || &compatible
  finish
endif
let g:loaded_complete_plugin = 1

" .: Scan the current buffer
" w: Scan buffers from other windows
" b: Scan buffers from the buffer list
" u: Scan buffers that have been unloaded from the buffer list
" t: Tag completion
" i: Scan the current and included files (SLOW)
set complete=.,w,b

" Previous values: noselect ,noinsert
" menuone,longest,preview
" longest,menuone
"
" Removed preview (because it opens a split)
set completeopt=longest,menu

" Determines the minimum width to use for the popup menu for Insert mode
if exists('&pumwidth')
  set pumwidth=20
endif

" Command mode completion
set wildmode=list:longest,full

" display completion matches in a status line
set wildmenu

" The below mapping will change the behavior of the <Enter> key when the popup
" menu is visible. In that case the Enter key will simply select the
" highlighted menu item, just as <C-Y> does.
" https://vim.fandom.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
"
" I have also added a hack Hack to preserve indentation (<Space><BS>)
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR><Space><BS>"

" Remap code completion to Ctrl+Space (doesn't work with youcompleteme)
inoremap <C-Space> <C-x><C-o>

augroup CompletePlugin
  autocmd!
  " Close preview window after completion
  autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | silent! pclose | endif
augroup END
