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
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
" SOFTWARE.

" buffer.vim
" Requires: buffer.vim

if exists('g:loaded_buffer_plugin') || &compatible
  finish
endif
let g:loaded_buffer_plugin = 1

command! CloseHiddenBuffers call buffer#close_hidden()
command! DiffSaved call buffer#diff_with_saved()
command! DiffUndo call buffer#diff_with_undo()
command! -nargs=1 -complete=file Diff tab vertical diffsplit <f-args>
command! -bar -bang -nargs=1 -complete=file Edit call buffer#goto_bufname_or_edit(<f-args>, <bang>0)
command! -bar -bang -nargs=1 -complete=file E call buffer#goto_bufname_or_edit(<f-args>, <bang>0)
command! -bar -bang -nargs=1 -complete=customlist,vim#complete_command_sibling_files EC call buffer#goto_bufname_or_edit(<f-args>, <bang>0, 1)
" command! WhiteSpaceDelete call buffer#delete_white_space()
command! -bar -nargs=1 IndentChange call buffer#change_tab(<f-args>)

nnoremap ,cs :call buffer#special_chdir('..')<CR>
nnoremap ,cp :pwd<CR>

" Quickly replace the word that is under the cursor or the currently selected text.
nnoremap <silent> ,rr :call buffer#replace_word_under_cursor()<CR>
vnoremap <silent> ,rr :call buffer#replace_word_under_cursor()<CR>
