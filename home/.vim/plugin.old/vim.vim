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

" vim.vim

if exists('g:loaded_vim_vim_plugin') || &compatible
  finish
endif
let g:loaded_vim_vim_plugin = 1

" Watch the size of VimInfo
"
augroup VimVimPlugin
  autocmd!
  autocmd VimEnter * call vim#viminfo_watchdog()
  autocmd VimEnter * call vim#disable_mouse()
  autocmd VimEnter * if exists(':NoMatchParen') | execute ':NoMatchParen' | endif

  " Save all modified files when the focus is lost.
  autocmd FocusLost * call vim#save_all()
  autocmd BufReadPost if &key !=# '' | redraw | endif
augroup END

" Key mappings
call vim#map_command('nvit', '<C-q>', 'call vim#quit()')

nnoremap <silent> ,cg :call vim#chdir_project_dir()<CR>

command! -nargs=1 FindPre call vim#find_pre(<f-args>)

function! s:source(arg, bang) abort
  let l:path = fnamemodify(expand(a:arg), ':t')
  if l:path[-4:] !=# '.vim' && l:path[0:len('.vimrc')-1] !=# '.vimrc'
    echomsg 'Only *.vim files can be sourced...'
    return
  endif
  execute 'source' . ((a:bang) ? '!' : '') . ' ' . a:arg
endfunction
command! -bar -bang -nargs=1 Source call <SID>source(<f-args>, <bang>0)
