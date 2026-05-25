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

" fonts.vim

if exists('g:loaded_fonts_plugin') || &compatible
  finish
endif

let g:loaded_fonts_plugin = 1

" \ {'name': 'Source Code Pro Bold'},
" \ {'name': 'Source Code Pro Semibold'},
" \ {'name': 'Source Code Pro Black'},
" \ {'name': 'Source Code Pro Medium'},
" \ {'name': 'Source Code Pro'},
" \ {'name': 'Iosevka Term Ultra-Bold Expanded'},
" \ {'name': 'Iosevka Term Semi-Bold Expanded'},
" \ {'name': 'Iosevka Term Bold Expanded'},
let g:list_favorite_fonts = get(g:,
      \ 'list_favorite_fonts', [
      \   {'name': 'Iosevka Term Ultra-Bold'},
      \   {'name': 'Iosevka Term Bold'},
      \   {'name': 'Iosevka Term'},
      \   {'name': 'Inconsolata Bold'},
      \   {'name': 'Fira Code Bold'},
      \   {'name': 'Hack Bold'},
      \   {'name': 'JetBrains Mono Bold'},
      \   {'name': 'JetBrains Mono Semi-Bold'},
      \   {'name': 'mononoki Bold'},
      \   {'name': 'IBM Plex Mono Bold'},
      \   {'name': 'IBM Plex Mono Semi-Bold'},
      \   {'name': 'Courier Prime Bold'},
      \   {'name': 'Cascadia Code Bold'},
      \   {'name': 'Cascadia Code Semi-Bold'},
      \   {'name': 'DejaVu Sans Mono'},
      \   {'name': 'Inconsolata Regular'},
      \   {'name': 'Luxi Mono Bold'},
      \   {'name': 'Courier New Bold'}
      \ ])

let g:font_list = []

command! -nargs=0 FontNext call font#next()
command! -nargs=0 FontPrevious call font#previous()
command! -nargs=0 FontDefault call font#default()
command! -nargs=0 FontName echo &guifont

if has('gui_running')
  nnoremap <silent> ,FN :FontName<CR>
  nnoremap <silent> ,FG :FontNext<CR>
  nnoremap <silent> ,FD :FontPrevious<CR>

  augroup Fonts
    autocmd!
    autocmd VimEnter * nested call font#init()
  augroup END
endif
