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

" tabhelp.vim

function! tabhelp#open(word, ...) abort
  let l:buf_cmd = ''
  let l:bufname_prefix = ''

  if len(a:000) > 0
    let l:tabhelpprg = a:1['tabhelpprg']
    let l:bufname_prefix = a:1['bufname_prefix']
  endif

  if l:bufname_prefix ==# ''
    let l:bufname_prefix = 'help:'
  endif

  if !exists('l:tabhelpprg')
    let l:tabhelpprg = get(b:, 'tabhelpprg', '')
    if l:tabhelpprg ==# ''
      normal! K
      return
    endif
  endif

  if l:tabhelpprg[0] ==# ':'
    if stridx(l:tabhelpprg, '%s') ==# -1
      execute l:tabhelpprg
    else
      execute printf(l:tabhelpprg, fnameescape(a:word))
    endif
    return
  else
    let l:buf_cmd = 'silent read! '
    if stridx(l:tabhelpprg, '%s') ==# -1
      let l:buf_cmd .= l:tabhelpprg
    else
      let l:buf_cmd .= printf(l:tabhelpprg, shellescape(a:word))
    endif
  endif

  execute printf('silent tabnew %s%s',
        \        l:bufname_prefix,
        \        fnameescape(a:word))

  setlocal modifiable
  silent normal! ggdG
  silent normal! 1Gdd
  if l:buf_cmd !=# ''
    execute l:buf_cmd
  endif
  silent normal! gg0
  setlocal nomodifiable
  setlocal noswapfile
  setlocal nowrap
  " setlocal nonumber
  setlocal nomodified
  setlocal buftype=nofile
  setlocal bufhidden=delete
  " if exists('&relativenumber')
  "   setlocal norelativenumber
  " endif
  if exists('&signcolumn')
    setlocal signcolumn=no
  endif
  setlocal nofoldenable
  setlocal foldcolumn=0
endfunction

function! tabhelp#dict(text) abort
  let l:word = trim(input('Enter word or phrase: ', a:text))
  call tabhelp#open(l:word, {'tabhelpprg': 'timeout -k 3 3 sdcv %s', 'bufname_prefix': 'Dict:'})

  setlocal modifiable
  setlocal wrap
  set syntax=stardict
  setlocal textwidth=78
  nnoremap <buffer> K :call tabhelp#open(expand('<cword>'))<CR>

  let l:winsaveview = winsaveview()
  " vint: next-line -ProhibitCommandWithUnintendedSideEffect
  " vint: next-line -ProhibitCommandRelyOnUser
  keeppatterns silent! %substitute/\v([[:space:]]|^)[^[:space:]]+\.wav([[:space:]]|$)/\1\2/g

  " w: Trailing white space indicates a paragraph continues in the next line. A
  " line that ends in a non-white character ends a paragraph.
  set formatoptions+=w
  normal! gggqG

  call winrestview(l:winsaveview)

  nnoremap <silent> <buffer> K :call tabhelp#dict(expand('<cword>'))<CR>
  setlocal nomodifiable
endfunction
