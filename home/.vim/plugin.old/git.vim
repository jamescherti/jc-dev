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

" git.vim

if exists('g:loaded_git_plugin') || &compatible
  finish
endif
let g:loaded_git_plugin = 1

augroup GitPlugin
  autocmd!
  autocmd BufWritePost * let b:git_branch = git#branch()
augroup END

command! -nargs=? GitFind call git#find(<f-args>)

nnoremap ,gitf :GitFind<Space>

function! s:git_ci() abort
  call git#run_from_repo('git-ci')
endfunction

function! s:git_cip() abort
  call unix#load_ssh_agent()
  call git#run_from_repo('git-cip')
endfunction

function! s:git_gpl() abort
  call unix#load_ssh_agent()
  call git#run_from_repo('git pullall')
endfunction

command! -bar -nargs=0 CI call <SID>git_ci()
command! -bar -nargs=0 CIP call <SID>git_cip()
command! -bar -nargs=0 GPL call <SID>git_gpl()

command! -bar -nargs=0 GitDiff call git#run_git_in_buffer('git diff')
command! -bar -nargs=0 GitShow
  \ call git#run_git_in_buffer('git show -p')
command! -bar -nargs=0 GitLog
  \ call git#run_git_in_buffer('git log -p --max-count 10')
command! -bar -nargs=0 GitShow call git#run_git_in_buffer('git show')
