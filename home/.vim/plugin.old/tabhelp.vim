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

" tabhep.vim

if exists('g:loaded_tabhelp_plugin') || &compatible
  finish
endif
let g:loaded_tabhelp_plugin = 1

command! Dict call tabhelp#dict('')

function! s:tabhelp_map(autocmd_type, file_type, command) abort
  execute printf('autocmd %s %s %s', a:autocmd_type, a:file_type, a:command)
  execute printf('autocmd %s %s nnoremap <buffer> K :call tabhelp#open(expand(''<cword>''))<CR>', a:autocmd_type, a:file_type)
endfunction

augroup TabHelpPlugin
  autocmd!

  command! -nargs=1 Word call Word(<f-args>)

  " call s:tabhelp_map('FileType', 'python ', 'let b:tabhelpprg = '':call jedi#show_documentation()''')
  call s:tabhelp_map('FileType', 'vim', 'let b:tabhelpprg = '':tab help %s''')
  call s:tabhelp_map('FileType', 'tmux', 'let b:tabhelpprg = ''tmux %s --help''')
  call s:tabhelp_map('FileType', 'yaml.ansible', 'let b:tabhelpprg = ''ansible-doc %s''')
  call s:tabhelp_map('BufReadPost', '*.txt', 'let b:tabhelpprg = ''echo 0 | timeout -k 5 5 sdcv %s''')
  call s:tabhelp_map('FileType', 'markdown ', 'let b:tabhelpprg = ''echo 0 | timeout -k 5 5 sdcv %s''')
  call s:tabhelp_map('FileType', 'sh,zsh,csh',
        \ 'if ! exists('':Man'') | runtime ftplugin/man.vim | endif | let b:tabhelpprg = '':tab Man %s''')

  " autocmd FileType python nnoremap <silent> <buffer> K :call jedi#show_documentation()<CR>
  " autocmd FileType python nnoremap <silent> <buffer> K :call jedi#show_documentation()<CR>

  command! -nargs=1 TabHelp call tabhelp#open(expand('<cword>'))

augroup END

nnoremap <silent> ,ew :call tabhelp#dict(expand('<cword>'))<CR>
nnoremap <silent> ,ed :call tabhelp#dict('')<CR>
