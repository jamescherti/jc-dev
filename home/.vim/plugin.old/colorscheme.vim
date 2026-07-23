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

" colorscheme.vim

if exists('g:loaded_colorscheme_plugin') || &compatible
  finish
endif
let g:loaded_colorscheme_plugin = 1

" \ {'name': 'solarized', 'background': 'light'},
" \ {'name': 'iceberg', 'background': 'light'},
" \ {'name': 'one', 'background': 'dark'},
" \ {'name': 'iceberg', 'background': 'dark'},
let g:colorscheme_list_favorite = [
      \ {'name': 'tomorrow-night-deepblue', 'background': 'dark'},
      \
      \ {'name': 'gruvbox', 'background': 'light'},
      \ {'name': 'PaperColor', 'background': 'light'},
      \
      \ {'name': 'nord', 'background': 'dark'},
      \ {'name': 'molokai', 'background': 'dark'},
      \ {'name': 'PaperColor', 'background': 'dark'},
      \ {'name': 'atom-dark-256', 'background': 'dark'},
      \ {'name': 'gruvbox', 'background': 'dark'},
      \ ]

" let g:colorscheme_list_favorite = colorscheme#get_all()

function! s:echo_colorscheme() abort
  let l:colors_name = '<unknown>'
  if exists('g:colors_name')
    let l:colors_name = g:colors_name
  endif
  echo 'Colorscheme: ' . l:colors_name . ' (' . &background . ')'
endfunction

command! ColorSchemeNext call colorscheme#next() | call <SID>echo_colorscheme()
command! ColorSchemePrevious call colorscheme#previous() | call <SID>echo_colorscheme()

"if has('gui_running')
nnoremap ,cv :ColorSchemeNext<CR>
nnoremap ,cx :ColorSchemePrevious<CR>
"endif
