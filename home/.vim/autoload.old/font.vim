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

" fonts.vim

" Return [fontname, fontsize]

function! s:set_font_index(index) abort
  if a:index < 0 || a:index > len(g:list_favorite_fonts)
    throw 'font.vim: s:set_font_index(): invalid index ' . a:index
    return
  endif

  let l:favorite_font = g:list_favorite_fonts[a:index]

  let l:font_name = FontParseGuiFont(l:favorite_font['name'])[0]
  let l:font_size = FontParseGuiFont(&guifont)[1]

  " if has_key(l:favorite_font, 'add_size')
  "   let l:font_size += l:favorite_font['add_size']
  " endif

  call s:font_modify(l:font_name, l:font_size)
  let g:font_current_index = a:index
endfunction

function! s:font_modify(font_name, font_size) abort
  if !((has('gui_gtk3') || has('gui_gtk2')) && has('gui_running'))
    throw 'font.vim: FontParseGuiFont(): the only GUI that are supported are Gtk 2 and Gtk 3.'
  endif

  let &guifont = a:font_name . ' ' . a:font_size
  " execute 'set guifont=' . escape(a:font_name . ' ' . a:font_size, ' \"')
  " wincmd =
  redraw
  echo 'Font: '. &guifont
endfunction

function! s:font_inc(inc) abort
  if len(g:list_favorite_fonts) ==# 0
    return
  endif

  let g:font_current_index = g:font_current_index + a:inc
  if g:font_current_index >= 0
    let l:font_current_index = g:font_current_index % len(g:list_favorite_fonts)
  else
    let l:font_current_index = len(g:list_favorite_fonts) - 1
  endif

  call s:set_font_index(l:font_current_index)
endfunction

function! s:font_error() abort
  if !has('gui_running')
    return 1
  endif

  return 0
endfunction

function! font#next() abort
  call font#init()

  if s:font_error()
    return
  endif
  call s:font_inc(1)
endfunction

function! font#previous() abort
  call font#init()

  if s:font_error()
    return
  endif
  call s:font_inc(-1)
endfunction

function! font#default() abort
  call font#init()

  if s:font_error()
    return
  endif
  call font#next()
endfunction

" let s:font_list_loaded = 0
" let s:font_init_done = 0
function! font#init() abort
  " if s:font_init_done
  "   return
  " else
  "   let s:font_init_done = 1
  " endif

  let g:font_current_index = get(g:, 'current_font_index', -1)

  if s:font_error()
    return 0
  endif

  " Auto detect the font index
  "if !empty(&guifont)
    let l:old_font_current_index = g:font_current_index
    let g:font_current_index = s:font_find_index()
    if g:font_current_index !=# l:old_font_current_index
      silent call s:font_inc(0)
    endif
  "endif

  " Load the list of fonts
  " if !s:font_list_loaded
  "   let g:font_list = font#list()

  "   let l:list_favorite_fonts = []
  "   for l:favorite_font in g:list_favorite_fonts
  "     let l:favorite_font['name'] = trim(l:favorite_font['name'])
  "     if index(g:font_list, l:favorite_font['name'], 0, 1) >= 0
  "       call add(l:list_favorite_fonts, l:favorite_font)
  "     endif
  "   endfor
  "   let g:list_favorite_fonts = l:list_favorite_fonts

  "   let s:font_list_loaded = 1
  " endif

  return 1
endfunction

function! s:font_find_index() abort
  let [l:guifont_name, l:guifont_size] = FontParseGuiFont(&guifont)

  let l:index = 0
  while l:index < len(g:list_favorite_fonts)
    let l:favorite_font_name = g:list_favorite_fonts[l:index]['name']
    let [l:favorite_font_name, l:favorite_font_size] = FontParseGuiFont(l:favorite_font_name)

    if tolower(l:guifont_name) ==# tolower(l:favorite_font_name)
      return l:index
    endif

    let l:index += 1
  endwhile

  return -1
endfunction

"
" List  fonts
"
function! font#list_match(match) abort
  let l:result = []
  if s:font_error()
    return l:result
  endif

  for l:font in font#list()
    if match(tolower(l:font), tolower(a:match)) !=# -1
      call add(l:result, l:font)
    endif
  endfor
  return l:result
endfunction

function! font#list() abort
  let l:result = []
  if s:font_error()
    return l:result
  endif

  if has('win32') || !has('gui_running') || !executable('fc-list')
    return l:result
  endif

  " Search for monospaced fonts (spacing=100)
  let l:fclist_output = systemlist('fc-list :spacing=100')
  let l:style_var = 'style='

  for l:fclist_line in l:fclist_output
    let l:fclist_line_items = split(l:fclist_line, ':')
    let l:font_file = l:fclist_line_items[0]

    let l:list_font_names = split(l:fclist_line_items[1], ',')
    let l:font_name = trim(l:list_font_names[0])

    if len(l:fclist_line_items) <= 2
      if index(l:result, l:font_name) ==# -1
        call add(l:result, l:font_name)
      endif
      continue
    endif

    let l:font_style = l:fclist_line_items[2]
    if l:font_style[0:len(l:style_var)-1] ==# l:style_var
      for l:font_style in split(l:font_style[len(l:style_var):], ',')
        let l:font_name = l:font_name . ' ' . trim(l:font_style)
        if index(l:result, l:font_name) ==# -1
          call add(l:result, l:font_name)
        endif
      endfor
    endif
  endfor

  return l:result
endfunction
