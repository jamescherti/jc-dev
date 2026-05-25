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

" bufferconv.vim
" Buffer to text (e.g. PDF to text)
" Requires: buffer.vim

function! bufferconv#enable() abort
  if !&modifiable || &buftype !=# '' || buffer#is_empty()
    return
  endif

  let w:buffer_conv_enabled = get(w:, 'buffer_conv_enabled', 1)
  if ! w:buffer_conv_enabled
    return
  endif

  if !exists('b:buffer_conv_cmds') || len(b:buffer_conv_cmds) ==# 0
    echoerr 'Error: b:buffer_conv_cmds is empty.'
    return
  endif

  let b:buffer_conv_index = get(b:, 'buffer_conv_index', 0)
  execute 'keeppatterns silent %!' . b:buffer_conv_cmds[b:buffer_conv_index]
  set filetype=

  setlocal undolevels=-1
  call buffer#set_readonly()
  normal! gg0
  let b:buffer_conv_done = 1
endfunction

function! bufferconv#disable() abort
  let b:buffer_conv_done = get(b:, 'buffer_conv_done', 0)
  if ! b:buffer_conv_done
    return
  endif

  let l:bufname = bufname()
  let l:bufnr = bufnr('')
  enew!
  execute 'bwipeout! ' . l:bufnr

  let w:buffer_conv_enabled = 0
  execute 'silent edit ' . fnameescape(l:bufname)
  unlet w:buffer_conv_enabled
endfunction

function! bufferconv#next() abort
  let b:buffer_conv_done = get(b:, 'buffer_conv_done', 0)
  if ! b:buffer_conv_done
    return
  endif

  if !exists('b:buffer_conv_cmds') || len(b:buffer_conv_cmds) ==# 0
    echoerr 'Error: b:buffer_conv_cmds is empty.'
    return
  endif

  " save l:buffer_conv_index before it gets deleted by bufferconv#disable
  let l:buffer_conv_index = get(b:, 'buffer_conv_index', 0)

  call bufferconv#disable()
  let b:buffer_conv_index = (l:buffer_conv_index + 1) % len(b:buffer_conv_cmds)

  call bufferconv#enable()
endfunction
