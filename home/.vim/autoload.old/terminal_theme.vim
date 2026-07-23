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

" terminal_tango_theme.vim
" :help highlight-cterm
" :help term_setansicolors()

function! s:theme_tango_dark() abort
  " Gnome Terminal Tango Dark
  let g:terminal_ansi_colors_black = '#2E3436'
  let g:terminal_ansi_colors_darkred = '#CC0000'
  let g:terminal_ansi_colors_darkgreen = '#4E9A06'
  let g:terminal_ansi_colors_brown = '#C4A000'
  let g:terminal_ansi_colors_darkblue = '#3465A4'
  let g:terminal_ansi_colors_darkmagenta = '#75507B'  " dark purple
  let g:terminal_ansi_colors_darkcyan = '#06989A'  " dark turquoise
  let g:terminal_ansi_colors_lightgrey = '#D3D7CF'
  let g:terminal_ansi_colors_darkgrey = '#555753'
  let g:terminal_ansi_colors_red = '#EF2929'
  let g:terminal_ansi_colors_green = '#8AE234'
  let g:terminal_ansi_colors_yellow = '#FCE94F'
  let g:terminal_ansi_colors_blue = '#729FCF'
  let g:terminal_ansi_colors_magenta = '#AD7FA8'  " purple
  let g:terminal_ansi_colors_cyan = '#34E2E2' " turquoise
  let g:terminal_ansi_colors_white = '#EEEEEC'

  " Text and background: Gnome dark
  let g:terminal_ctermbg = 'black'
  let g:terminal_ctermfg = 'white'
  let g:terminal_statuslineterm_ctermbg = 'black'
  let g:terminal_statuslineterm_ctermfg = 'white'
  let g:terminal_statuslinetermnc_ctermbg = 'black'
  let g:terminal_statuslinetermnc_ctermfg = 'white'
  " let g:terminal_guibg = '#171421'
  let g:terminal_guibg = '#000000'
  let g:terminal_guifg = '#D0CFCC'
  let g:terminal_statuslineterm_guibg = g:terminal_guibg
  let g:terminal_statuslineterm_guifg = g:terminal_guifg
  let g:terminal_statuslinetermnc_guibg = g:terminal_guibg
  let g:terminal_statuslinetermnc_guifg = g:terminal_guifg
endfunction

function! s:theme_tango_dark_tilix() abort
  " Tilix
  let g:terminal_ansi_colors_black = '#000000'
  let g:terminal_ansi_colors_darkred = '#cc0000'
  let g:terminal_ansi_colors_darkgreen = '#4d9a05'
  let g:terminal_ansi_colors_brown = '#c3a000'
  let g:terminal_ansi_colors_darkblue = '#3464a3'
  let g:terminal_ansi_colors_darkmagenta = '#754f7b'  " dark purple
  let g:terminal_ansi_colors_darkcyan = '#05979a'  " dark turquoise
  let g:terminal_ansi_colors_lightgrey = '#d3d6cf'
  let g:terminal_ansi_colors_darkgrey = '#89e234'
  let g:terminal_ansi_colors_red = '#ef2828'
  let g:terminal_ansi_colors_green = '#89e234'
  let g:terminal_ansi_colors_yellow = '#fbe84f'
  let g:terminal_ansi_colors_blue = '#729ecf'
  let g:terminal_ansi_colors_magenta = '#ac7ea8'  " purple
  let g:terminal_ansi_colors_cyan = '#34e2e2' " turquoise
  let g:terminal_ansi_colors_white = '#ffffff'

  let g:terminal_ctermbg = 'black'
  let g:terminal_ctermfg = 'white'
  let g:terminal_guibg = '#111111'
  let g:terminal_guifg = '#eeeeee'

  let g:terminal_statuslineterm_ctermbg = 'black'
  let g:terminal_statuslineterm_ctermfg = 'white'
  let g:terminal_statuslineterm_guibg = '#282828'
  let g:terminal_statuslineterm_guifg = '#6d6d6d'

  let g:terminal_statuslinetermnc_ctermbg = 'black'
  let g:terminal_statuslinetermnc_ctermfg = 'white'
  let g:terminal_statuslinetermnc_guibg = '#282828'
  let g:terminal_statuslinetermnc_guifg = '#6d6d6d'
endfunction

function! terminal_theme#update() abort
  if g:terminal_colorscheme ==# 'tango'
    call s:theme_tango_dark()
  elseif g:terminal_colorscheme ==# 'tilix_tango'
    call s:theme_tango_dark_tilix()
  else
    " Default
    call s:theme_tango_dark()
  endif

  let g:terminal_ansi_colors = [
    \ g:terminal_ansi_colors_black,
    \ g:terminal_ansi_colors_darkred,
    \ g:terminal_ansi_colors_darkgreen,
    \ g:terminal_ansi_colors_brown,
    \ g:terminal_ansi_colors_darkblue,
    \ g:terminal_ansi_colors_darkmagenta,
    \ g:terminal_ansi_colors_darkcyan,
    \ g:terminal_ansi_colors_lightgrey,
    \ g:terminal_ansi_colors_darkgrey,
    \ g:terminal_ansi_colors_red,
    \ g:terminal_ansi_colors_green,
    \ g:terminal_ansi_colors_yellow,
    \ g:terminal_ansi_colors_blue,
    \ g:terminal_ansi_colors_magenta,
    \ g:terminal_ansi_colors_cyan,
    \ g:terminal_ansi_colors_white
    \ ]

  execute printf(
        \ 'highlight Terminal ctermbg=%s ctermfg=%s guibg=%s guifg=%s',
        \ g:terminal_ctermbg, g:terminal_ctermfg, g:terminal_guibg,
        \ g:terminal_guifg
        \ )
  execute printf(
        \ 'highlight StatusLineTerm ctermbg=%s ctermfg=%s guibg=%s guifg=%s',
        \ g:terminal_statuslineterm_ctermbg, g:terminal_statuslineterm_ctermfg,
        \ g:terminal_statuslineterm_guibg, g:terminal_statuslineterm_guifg
        \ )
  execute printf(
        \ 'highlight StatusLineTermNC ctermbg=%s ctermfg=%s guibg=%s guifg=%s',
        \ g:terminal_statuslinetermnc_ctermbg, g:terminal_statuslinetermnc_ctermfg,
        \ g:terminal_statuslinetermnc_guibg, g:terminal_statuslinetermnc_guifg
        \ )
endfunction
