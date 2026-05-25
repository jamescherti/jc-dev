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

" unix.vim

if exists('g:loaded_unix_plugin') || &compatible
  finish
endif
let g:loaded_unix_plugin = 1

let g:unix_find_ignore_dirs = ['.git']

command! -bar -bang -nargs=1 -complete=customlist,vim#complete_command_sibling_files Rename :call unix#smartmv(<f-args>, <bang>0, 0)
command! -bar -bang -nargs=1 -complete=file Move :call unix#smartmv(<f-args>, <bang>0, 1)
command! -bar -bang -nargs=1 -complete=customlist,vim#complete_command_sibling_files Copy :call unix#copy(<f-args>, <bang>0, 0)
command! -bar -bang -nargs=1 -complete=customlist,vim#complete_command_sibling_files BCopy :call unix#copy(<f-args>, <bang>0, 1)
command! -bar -bang Delete call unix#delete(<bang>0)
command! -bar -bang Unlink call unix#unlink(<bang>0)
command! -bar -bang Remove call unix#remove(<bang>0)
command! -bar -bang Reload call unix#reload(<bang>0)
command! -bar -bang -nargs=1 Chmod call unix#chmod(<f-args>, <bang>0)
command! -bar -bang -nargs=0 XdgOpen call unix#xdg_open(expand('%'), <bang>0)
command! -nargs=1 UnixFind call unix#find(<f-args>)
command! -nargs=1 -complete=file Mkdir call unix#mkdir(<f-args>)
command! -nargs=1 -complete=file XdgOpen call unix#xdg_open(<f-args>)
command! -nargs=1 Which call unix#edit_command(<f-args>)

" Key mappings
" nnoremap <silent> ,to :call unix#xdg_open(expand('%:p'))<CR>
