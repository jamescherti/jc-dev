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

" bufferconv.vim

if exists('g:loaded_bufferconv_plugin') || &compatible
  finish
endif
let g:loaded_bufferconv_plugin = 1

augroup BufferConv
  autocmd!
  " autocmd BufReadPost *.html,*.htm let b:buffer_conv_cmds = ['html2text'] | call bufferconv#enable() | set filetype=markdown
  " autocmd BufReadPre *.pdf setlocal binary
  autocmd BufReadPost *.pdf
        \ let b:buffer_conv_cmds = [
        \   'pdftotext -layout ' . shellescape(bufname()) . ' -',
        \   'pdftotext ' . shellescape(bufname()) . ' -'
        \ ] | call bufferconv#enable()
  autocmd BufReadPost *.odt
        \ let b:buffer_conv_cmds = [
        \   'odt2txt ' . shellescape(bufname())
        \ ] | call bufferconv#enable()
augroup END

command! -nargs=0 BufferConvDisable call bufferconv#disable()
command! -nargs=0 BufferConvNext call bufferconv#next()
