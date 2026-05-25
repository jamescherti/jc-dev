" Author: James Cherti
" URL: https://github.com/jamescherti/jc-dev
"
" Distributed under terms of the MIT license.
"
" Copyright (C) 2000-2026 James Cherti
"
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the ,Y4Software,Y!), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
"
" The above copyright notice and this permission notice shall be included in all
" copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED ,Y4AS IS,Y!, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
" SOFTWARE.

" file_explorer_switch.vim
" Requires: view.vim

scriptencoding utf-8

if exists('g:loaded_netrw_file_explorer_plugin') || &compatible
  finish
endif

" dirvish and others should do it
let g:loaded_netrw_file_explorer_plugin = 1

let s:file_explorer = 'dirvish'
" let s:file_explorer = 'netrw'
" let s:file_explorer = 'nerdtree'

" wildignore {{{

" Do not add files like pathaction.yaml or other
" (it will make :edit ignore them).
set wildignore=
set wildignore+=__pycache__,.pytest_cache,*.pyc,*.pyo,*.egg-info
set wildignore+=*.pyd,.coverage,.ipynb_checkpoints
set wildignore+=*.o,*.so,*.a,*.out,*.elf,*.obj,*.rbc,*.rbo,*.gem,*.dll,*.exe
set wildignore+=tags,cscope.out
set wildignore+=.DS_Store
set wildignore+=.hg,.git,.svn
set wildignore+=*.swp,*~,*#,*.bak

" set wildignore+=*.class,classes,*.jar,.lein-*
" set wildignore+=*.dSYM
" set wildignore+=*.ko,*.mod.c,*.order,modules.builtin
" set wildignore+=*.lib,*.exe,*.opt,*.ncb,*.plg,*.ilk
" set wildignore+=*.swp,
" set wildignore+=build,_build,export,pkgexp
" set wildignore+=target

" set wildignore+=.mypy_cache,.dmypy.json,dmypy.json
" set wildignore+=.tox,.eggs
" set wildignore+=.gitmodules,tags
"
" set wildignore+=*.log,*.retry
" set wildignore+=*.mo,*.pot
" set wildignore+=*.class,target,.idea
" set wildignore+=cython_debug,__pypackages__,htmlcov,.nox,.coverage,.coverage.*,.cache,nosetests.xml,coverage.xml,*.cover,cover,.hypothesis,cover  " Unit test / coverage reports
" set wildignore+=build,,dist,.Python,build,develop-eggs,downloads,eggs,lib,lib64,parts,sdist,var,wheels,python-wheels,.installed.cfg,*.egg,MANIFEST  " distribution / packaging
" set wildignore+=*.aux,*.out,*.toc,*.log,*.idx    " LaTeX intermediate files
" set wildignore+=*_aux,*.glg,*.glo,*.gls,*.ist    " LaTeX intermediate files
" set wildignore+=*.nlo,*.nls,*.pdf,*.bbl,*.dvi    " still LaTeX intermediate files
" set wildignore+=*.ilg,*.fdb_latexmk,*.synctex.gz " … LaTeX intermediate files
" set wildignore+=*.blg,*.ind                      " ……… LaTeX intermediate files
" set wildignore+=*.hi                             " Haskell linker files
" set wildignore+=*.log,local_settings.py,db.sqlite3,db.sqlite3-journal  " Django
" set wildignore+=.pyre
" set wildignore+=instance,.webassets-cache  " Flash
" set wildignore+=*.jpg,*.bmp,*.gif,*.webp,*.png,*.jpeg   " binary images
" set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz,*.tar.zst,*.7z
" set wildignore+=*.manifest,*.spec  " PyInstaller
" set wildignore+=pip-log.txt,pip-delete-this-directory.txt  " Installer logs
" set wildignore+=.env,.venv,env,venv,ENV,env.bak,venv.bak,
" set wildignore+=*.luac                           " Lua byte code

" }}}

let g:loaded_netrwPlugin = 1
if s:file_explorer ==# 'netrw'
  " Netrw {{{
  let g:netrw_banner = 0
  let g:netrw_liststyle = 3
  let g:netrw_sizestyle = 'h'
  let g:netrw_sort_by = 'name'    " name, time, size
  let g:netrw_bufsettings = 'noma nomod nu nowrap ro nobl'  " Add number to netrw
  let g:netrw_hide = 1                   " hide by default

  let g:loaded_netrwPlugin = 0
  augroup NetrwHelpersPlugin
    autocmd!
    " Netrw switch
    autocmd BufLeave * call file_explorer_switch#netrw_file_explorer_save()
    autocmd BufReadPost * call file_explorer_switch#netrw_file_explorer_save()

    " Improvements
    autocmd Syntax netrw silent! unmap <buffer> -
    autocmd Syntax netrw silent! nnoremap <buffer> I :call file_explorer_switch#netrw_switch_hidden_files()<CR>
  augroup END

  " Init
  call file_explorer_switch#netrw_hidden_files_init()

  " Key mappings
  nnoremap <silent> ,fe :call file_explorer_switch#netrw_switch()<CR>
  nnoremap <silent> ,fE :execute ':Explore ' . fnameescape(getcwd())<CR>
  " }}}

"----------------------------------------------
" DIRVISH
"----------------------------------------------
elseif s:file_explorer ==# 'dirvish'
  let g:fileexplorerswitch_filetype = 'dirvish'
  let g:fileexplorerswitch_cmd = 'Dirvish .'

  let g:loaded_netrwPlugin = 1
  command! -nargs=? -complete=dir Explore Dirvish <args>
  command! -nargs=? -complete=dir Sexplore belowright split | silent Dirvish <args>
  command! -nargs=? -complete=dir Vexplore leftabove vsplit | silent Dirvish <args>

  "------------------------
  " Init Dirvish
  "------------------------
  function! s:setup_dirvish_mapping() abort
    if &filetype ==# 'dirvish'
      " Unmap file_explorer_switch#switch() mapping
      silent!  unmap <buffer> -
    else
      nnoremap <buffer> - <cmd>call file_explorer_switch#switch()<CR>
    endif
  endfunction

  augroup DirvishConfig
    autocmd!
    autocmd FileType dirvish silent! unmap <buffer> <C-p>
    " autocmd FileType dirvish call s:setup_dirvish_mapping()
  augroup END

  call file_explorer_switch#init()

  "------------------------
  " Hide dotfiles
  "------------------------
  let g:dirvish_dotfiles_hidden = 0

  function! s:dirvish_hide_dotfiles(hide) abort
    let g:dirvish_dotfiles_hidden = (a:hide) ? 1 : 0
    " let l:pos = getpos('.')
    Dirvish
    " call setpos('.', l:pos)
    echo 'Hide dotfiles: ' . g:dirvish_dotfiles_hidden
  endfunction

  function! s:dirvish_hide_dotfiles_setup() abort
    " The 'patch-8.2.1978' adds the feature: '<cmd>'
    if has('patch-8.2.1978')
      nnoremap <buffer> I <cmd>call <SID>dirvish_hide_dotfiles(!g:dirvish_dotfiles_hidden)<CR>
    else
      nnoremap <buffer> I :call <SID>dirvish_hide_dotfiles(!g:dirvish_dotfiles_hidden)<CR>
    endif

    if fnamemodify('~', ':p') ==# expand('%:p')
      let g:dirvish_dotfiles_hidden = 1
    endif

    if g:dirvish_dotfiles_hidden
      let l:pos = getpos('.')
      execute 'silent keeppatterns g@\v/\.[^\/]+/?$@d _'
      setlocal conceallevel=3
      call setpos('.', l:pos)
    endif
  endfunction

  augroup DirvishCustomConfig
    autocmd!

    " Map `t` to open in new tab.
    autocmd FileType dirvish
      \  nnoremap <silent><buffer> t :call dirvish#open('tabedit', 0)<CR>
      \ |xnoremap <silent><buffer> t :call dirvish#open('tabedit', 0)<CR>

    " Map `gr` to reload.
    autocmd FileType dirvish nnoremap <silent><buffer>
      \ <C-l> :<C-U>Dirvish<CR>

    " Map `gh` to hide dot-prefixed files.  Press `R` to "toggle" (reload).
    " autocmd FileType dirvish nnoremap <silent><buffer>
    "   \ gh :silent keeppatterns g@\v/\.[^\/]+/?$@d _<cr>:setl cole=3<cr>

    autocmd FileType dirvish call <SID>dirvish_hide_dotfiles_setup()
  augroup END

"----------------------------------------------
" NerdTREE
"----------------------------------------------
elseif s:file_explorer ==# 'nerdtree'
  " NerdTREE {{{
  function! s:NerdTreeFileExplorer() abort
    if !exists(':NERDTree')
      return
    endif

    " if g:NERDTree.IsOpen()
    "   silent NERDTreeClose
    "   return
    " endif

    let l:is_open = (g:NERDTree.IsOpen()) ? 1 : 0

    if l:is_open
      silent NERDTreeFocus
      return
    else
      silent NERDTree
    endif

    let l:file_path = expand('%:p')
    let l:cwd = fnamemodify(getcwd(), ':p')

    if !l:is_open && l:file_path[0:len(l:cwd) - 1] ==# l:cwd
      wincmd p
      silent! NERDTreeFind
    endif

    " NERDTreeRefreshRoot
  endfunction

  nnoremap <silent> ,fe :call <SID>NerdTreeFileExplorer()<CR>
  " }}}
endif
