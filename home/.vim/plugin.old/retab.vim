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

" retab.vim
" http://vim.wikia.com/wiki/Super_retab

if exists('g:loaded_retab_plugin') || &compatible
  finish
endif
let g:loaded_retab_plugin = 1

" command! -nargs=? -range=% RetabSpace2Tab call retab#indent_convert(<line1>,<line2>,0,<q-args>)
" command! -nargs=? -range=% RetabTab2Space call retab#indent_convert(<line1>,<line2>,1,<q-args>)
" command! -nargs=? -range=% RetabIndent call retab#indent_convert(<line1>,<line2>,&et,<q-args>)

command! -nargs=0 IndentSpaces4to2 call retab#indent_spaces_4_to_2()
