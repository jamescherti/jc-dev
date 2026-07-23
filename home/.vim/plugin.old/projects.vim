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

" projects.vim

if exists('g:loaded_projects_plugin') || &compatible
  finish
endif
let g:loaded_projects_plugin = 1

let g:vim_projects_dir = get(g:, 'vim_projects_dir', fnamemodify('~/.vim_projects', ':p'))
let g:vim_projects_path_sep = (!exists('+shellslash') || &shellslash) ? '/' : '\\'
let g:vim_projects_dir = g:vim_bundle_dir . g:vim_projects_path_sep . 'projects'

" let g:vim_helpers_path_sep = (has('win32') && !has('win32unix')) ? '\' : '/'
" let g:vim_helpers_dir = g:vim_bundle_dir . g:vim_helpers_path_sep . 'helpers'

" Key mappings
nnoremap <silent> ,z :call projects#choose()<CR>
