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

function! colorscheme#next() abort
  call colorscheme#inc(1)
endfunction

function! colorscheme#previous() abort
  call colorscheme#inc(-1)
endfunction

function! colorscheme#find_index() abort
  if !exists('g:colors_name') || g:colors_name ==# ''
    return -1
  endif

  if !exists('g:colorscheme_list_favorite')
    return -1
  endif

  let l:index = 0
  while l:index < len(g:colorscheme_list_favorite)
    if g:colorscheme_list_favorite[l:index]['name'] ==# g:colors_name &&
          \ g:colorscheme_list_favorite[l:index]['background'] ==# &background
      return l:index
    endif
    let l:index += 1
  endwhile

  return -1
endfunction

function! colorscheme#inc(inc) abort
  if !exists('g:colorscheme_list_favorite') || len(g:colorscheme_list_favorite) ==# 0
    return
  endif

  if !exists('g:current_colorscheme_index')
    let g:current_colorscheme_index = colorscheme#find_index()
  endif

  let g:current_colorscheme_index = g:current_colorscheme_index + a:inc
  if g:current_colorscheme_index >= 0
    let g:current_colorscheme_index = g:current_colorscheme_index % len(g:colorscheme_list_favorite)
  else
    let g:current_colorscheme_index = len(g:colorscheme_list_favorite) - 1
  endif

  call colorscheme#set(g:current_colorscheme_index)
endfunction

function! colorscheme#set(index)
  let l:name = g:colorscheme_list_favorite[a:index]['name']
  let l:background = g:colorscheme_list_favorite[a:index]['background']

  let &background = l:background
  execute 'colorscheme ' . l:name

  if !exists('g:colors_name')
    let g:colors_name = l:name
  endif

  if exists('g:syntax_on') && g:syntax_on
    let l:view = view#mkview(win_getid())
    " Fix syntax highlighting issue when the color scheme is changed
    syntax off
    syntax on
    call view#loadview(l:view, win_getid(), 0)
  endif

  redraw
endfunction

function! colorscheme#exists(name) abort
    return !empty(globpath(&runtimepath, 'colors/' . a:name . '.vim'))
endfunction

function! colorscheme#get_all() abort
  let l:list_color_schemes = uniq(sort(map(
        \  globpath(&runtimepath, 'colors/*.vim', 0, 1),
        \  'fnamemodify(v:val, ":t:r")'
        \)))

  let l:colorscheme_list_favorite = []
  for l:name in l:list_color_schemes
    call add(l:colorscheme_list_favorite, {'name': l:name, 'background': 'light'})
    call add(l:colorscheme_list_favorite, {'name': l:name, 'background': 'dark'})
  endfor

  return l:colorscheme_list_favorite
endfunction
