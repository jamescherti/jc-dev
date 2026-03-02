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

" Alt Fixes {{{

" Selection {{{

" Tell vim to remember certain things when we exit
" if has('nvim')
"   set viminfo='10,\"100,:100,n~/.nviminfo
" else
"   set viminfo='100,\"300,<50,s10,h,:20,n~/.viminfo
" endif

nnoremap <silent> ,a :ALENext<CR>
nnoremap <silent> ,A :ALEPrevious<CR>
nnoremap <silent> ,ea :ALEFix<CR>

nnoremap ! :!

nnoremap <leader>ee :call pathaction#run('main')<CR>
nnoremap <leader>ei :call pathaction#run('install')<CR>

autocmd FileType vim
  \ setlocal foldmethod=marker

if has('patch-8.2.1978')
  nnoremap > <cmd>call <SID>display_error('cnext')<CR>
  nnoremap < <cmd>call <SID>display_error('cprevious')<CR>
  nnoremap <C-S->> <cmd>call <SID>display_error('cnfile')<CR>
  nnoremap <C-S-<> <cmd>call <SID>display_error('cpfile')<CR>
else
  nnoremap > :call <SID>display_error('cnext')<CR>
  nnoremap < :call <SID>display_error('cprevious')<CR>
  nnoremap <C-S->> :call <SID>display_error('cnfile')<CR>
  nnoremap <C-S-<> :call <SID>display_error('cpfile')<CR>
endif

" indicators
function! s:inv_cursorline_cursorcolumn(colorcolumn_enabled) abort
  if &cursorline
    setlocal nocursorline nocursorcolumn colorcolumn=0
  else
    setlocal cursorline cursorcolumn
    if a:colorcolumn_enabled
      if &textwidth !=# 0
        let &colorcolumn=&textwidth
      else
        let &colorcolumn=81
      endif
    endif
  endif
endfunction

nnoremap <F6> :echo "Use ,ec"<CR>
nnoremap <F7> :echo "Use ,ec"<CR>
" nnoremap <F6> :setlocal colorcolumn=0 nocursorline nocursorcolumn<CR>
" nnoremap <F7> :setlocal cursorline cursorcolumn<CR>

nnoremap ,ec <cmd>call <SID>inv_cursorline_cursorcolumn(0)<CR>
nnoremap ,eC <cmd>call <SID>inv_cursorline_cursorcolumn(1)<CR>

"set selection=exclusive
" set virtualedit=onemore
set virtualedit=all

if !exists('g:colors_name') || empty(g:colors_name)
  let g:colors_name=''
  set background=dark
  silent! color tomorrow-night-deepblue
  if !exists('g:colors_name') || g:colors_name !=# 'tomorrow-night-deepblue'
    color blue
  endif
  syntax on
endif

" l = one more
nnoremap $ $l

""" vimrc.local{{{
if filereadable(fnamemodify('~/.vimrc.local', ':p'))
  source ~/.vimrc.local
endif

if filereadable(fnamemodify('~/.vimrc.local2', ':p'))
  source ~/.vimrc.local2
endif
""" }}}


 " }}}


" ------------------> end
  finish

  " autocmd FileType python call buffer#change_tab(4) |
  "       \ setlocal foldnestmax=2 cursorcolumn

  " autocmd FileType vim,python,sh,cpp,c,lua,puppet,json,ruby,yaml,yaml.ansible
  "   \ call buffer#auto_close_all_delimiters() |
  "   \ setlocal foldlevel=1 foldmethod=indent
  "
  " autocmd FileType text,sh,puppet,json,ruby,yaml,yaml.ansible,vim
  "   \ call buffer#change_tab(2)
  " autocmd FileType lua,markdown call buffer#change_tab(4)


" Keyboard mapping {{{

" Mappings {{{

" The following key mapping seems to make gvim hide its window
nnoremap <C-z> <Nop>

nnoremap ~ :edit ~/<CR>

" More precise restore map
nnoremap ' `

" yank/paste/delete to a different register than the main one
" because the main one is sometimes modified by plugins (UltiSnips?)
" or when the user presses the wrong combination of keys.
let g:alternative_register = 'm'

" Does not work: gp

" for item in ['<C-h>']
"   execute printf('vnoremap %s "%s%s', item, g:alternative_register, item)
" endfor

" 'x',
" for item in ['y', 'Y', 'X', 'd', 'D', 'c', 'C', 'zy', 's', 'S', 'p', 'P', 'zp', 'zP', '[P', ']P', '[p', ']p', 'gP']
"   execute printf('nnoremap %s "%s%s', item, g:alternative_register, item)
"   execute printf('vnoremap %s "%s%s', item, g:alternative_register, item)
" endfor

" Python: form string to list
"nnoremap ,tls :s/.*/\='['.join(map(split(submatch(0), '\s\+'), '"''" .. v:val .. "''"'), ', ').']'<CR>
"nnoremap ,tld :s/.*/\='['.join(map(split(submatch(0), '\s\+'), '''"'' .. v:val .. ''"'''), ', ').']'<CR>

" Never use an important character like \ or , in pastetoggle because it will
" be slowed down in insert mode.
" set pastetoggle=,ep

" Better visual search ()
"
" \%V   Match inside the Visual area.
" \M    Pattern will be interpreted as if 'nomagic' is used.
vnoremap ,s :s/\%V\M
nnoremap ,s :%s/\M
nnoremap ,S :s/\M

nnoremap ,h :setlocal invhlsearch<CR>
nnoremap <silent> <Leader>ic :silent setlocal invignorecase ignorecase?<CR>
nnoremap <silent> <Leader>wr :silent setlocal invwrap wrap?<CR>

" Alt + character to escape it
for s:item in ['[', ']', '.', '/', '\']
  execute 'cnoremap <A-' . s:item . '> \' . s:item
endfor

" Cannot add \ because it adds a delay to command line
for s:item in ['{', '}', '(', ')', '+', '~', '^', '\|', '?', '$', '*']
  execute 'cnoremap <A-' . s:item . '> \' . s:item
endfor

function! ExprHelpCommand() abort
  if &filetype ==# 'help'
    return ":help\<Space>"
  else
    return ":tab help\<Space>"
  endif
endfunction

nnoremap <expr> ,x ExprHelpCommand()

" n will search and zz will center the cursor
nnoremap <A-n> nzz
nnoremap <A-N> Nzz

" Disable visual mode
" noremap Q <Nop>
noremap Q :close<CR>

" Disable decrease number
noremap <C-x> <Nop>
noremap <C-a> <Nop>

" Disable upper case / lower case
vnoremap u <Nop>
vnoremap u <Nop>
vnoremap ,u u
vnoremap ,u U

nnoremap <silent> <M-+> :setlocal foldlevel+=1<CR>
nnoremap <silent> <M-=> :setlocal foldlevel+=2<CR>
nnoremap <silent> <M--> :setlocal foldlevel-=1<CR>
nnoremap ,fi :setlocal foldmethod=indent<CR>
nnoremap ,ft <cmd>setlocal foldmethod=expr foldexpr=folding#foldexpr_indent(v:lnum)<CR>
nnoremap ,fm :setlocal foldmethod=marker<CR>

nnoremap ,lf :setlocal spell spelllang=fr<cr>
nnoremap ,le :setlocal spell spelllang=en<cr>
nnoremap ,ln :setlocal spell spelllang=<cr>

" Prevent vim from printing the message 'Type  :qa  and press <Enter> to exit Vim'
nnoremap <C-c> <Nop>

" Improve Ctrl-C (hjklmode improvement)
inoremap <C-c> <Esc>
nnoremap <Enter> <Nop>

inoremap <A-b> <C-o>b
inoremap <A-w> <C-o>w
inoremap <A-S-b> <C-o>B
inoremap <A-S-w> <C-o>W

" Alt+shift+h
nnoremap <A-S-h> B
nnoremap <A-S-l> W
inoremap <A-S-h> <C-Left>
inoremap <A-S-l> <C-Right>
cnoremap <A-S-h> <C-Left>
cnoremap <A-S-l> <C-Right>

nnoremap gbc yyp^y$V:!perl -e '$x = <C-R>"; print $x'<CR>-y0j0P
vnoremap gbc yo<Esc>p^y$V:!perl -e '$x = <C-R>"; print $x'<CR>-y0j0P

" Other folding improvements
nnoremap zM zxzM

" Indent and paste
nnoremap <A-p> ]p
inoremap <A-p> <Esc>]p
nnoremap <A-P> ]P
inoremap <A-P> <Esc>]P

" Next and previous buffer

function! s:display_error(command) abort
  try
    execute a:command
  catch
    echo string(v:exception)
    return
  endtry

  if foldclosed('.') != -1
    " zv    View cursor line: Open just enough folds to make the line in which
    " the cursor is located not folded.
    "
    " zz    Like "z.", but leave the cursor in the same column.  Careful: If
    " caps-lock is on, this command becomes ZZ": write buffer and exit!
    normal! zvzz
  else
    " zz    Like "z.", but leave the cursor in the same column.  Careful: If
    " caps-lock is on, this command becomes ZZ": write buffer and exit!
    normal! zz
  endif
endfunction

" The 'patch-8.2.1978' adds the feature: '<cmd>'
" nnoremap <silent> > :silent cnext<CR>
" nnoremap <silent> < :silent cprevious<CR>
nnoremap [b :bprevious<CR>
nnoremap ]b :bnext<CR>
nnoremap [B :bfirst<CR>
nnoremap ]B :blast<CR>

nnoremap <A-S-Left> :vertical resize -1<CR>
nnoremap <A-S-Right> :vertical resize +1<CR>
nnoremap <A-S-Up> :resize -1<CR>
nnoremap <A-S-Down> :resize +1<CR>

nnoremap <silent> ,n :enew<CR>

" nnoremap ,i :call PythonAddImport(expand('<cword>'))<CR>

if has('gui_running')
  if executable('wmctrl')
    nnoremap <silent> <F11>
      \ :call system('wmctrl -x -a gvim.Gvim ' .
      \              '-b toggle,fullscreen')<CR>:redraw!<CR>
    nnoremap <silent> <F10>
      \ :call system('wmctrl -x -a gvim.Gvim -b ' .
      \              'toggle,maximized_vert,maximized_horz')<CR>:redraw!<CR>
  else
    nnoremap <F11> :echo 'Error: "wmctrl" does not exist.'<CR>
  endif
endif

inoremap <CR> <CR><Space><BS>
inoremap <C-CR> <C-CR><Space><BS>
inoremap <S-CR> <S-CR><Space><BS>
inoremap <C-S-CR> <C-S-CR><Space><BS>

inoremap <kEnter> <kEnter><Space><BS>
inoremap <C-kEnter> <C-kEnter><Space><BS>
inoremap <S-kEnter> <S-kEnter><Space><BS>
inoremap <C-S-kEnter> <C-S-kEnter><Space><BS>

inoremap <C-j> <C-j><Space><BS>

" }}}

" Plugins {{{

" Select the register with `
" nnoremap ` "

" nnoremap <silent> ,rv :source ~/.vimrc<CR>
"   \ :call system('wmctrl -x -a gvim.Gvim -b toggle,maximized_vert,' .
"   \ 'maximized_horz')<CR>:call system('wmctrl -x -a gvim.Gvim -b ' .
"   \ 'add,maximized_vert,maximized_horz')<CR>:echo 'Success: ~/.vimrc ' .
"   \ 'reloaded successfully'<CR>:redraw!<CR>
" nnoremap <silent> <C-p> :FZF<CR>

" Use S instead
" vmap ,s"   <Plug>VSurround"
" vmap ,s'   <Plug>VSurround'
vmap S{   <Plug>VSurround}gv
vmap S(   <Plug>VSurround)gv
vmap S[   <Plug>VSurround]gv

nnoremap <silent> ,ggt
  \ :packadd gitgutter<CR>:GitGutterToggle<CR>:echo "GitGutterToggle"<CR>
nnoremap <silent> ,gge
  \ :packadd gitgutter<CR>:GitGutterEnable<CR>:echo "GitGutterEnable"<CR>
nnoremap <silent> ,ggn
  \ :packadd gitgutter<CR>:GitGutterNextHunk<CR>:echo "GitGutterNextHunk"<CR>
nnoremap <silent> ,ggp
  \ :packadd gitgutter<CR>:GitGutterPrevHunk<CR>:echo "GitGutterPrevHunk"<CR>

" For an unknown reason, gg opens the first fold. This mapping fixes it
nnoremap <silent> gg :normal! gg<CR>

" }}}

" END : keyboard mapping }}}


" autocmd BufRead,BufNewFile * :call IgnoreCamelCaseSpell()
" Only do this part when compiled with support for autocommands.


" Gvim display corruption on NVIDIA
" https://bugs.launchpad.net/ubuntu/+source/vim/+bug/572863
" https://bugs.launchpad.net/ubuntu/+source/vim/+bug/386394
" set ttyscroll=500
" set ttyscroll=1500

" The time in milliseconds for redrawing the display.  This applies to
" searching for patterns for 'hlsearch', |:match| highlighting and syntax
" highlighting.
" When redrawing takes more than this many milliseconds no further
" matches will be highlighted.
" For syntax highlighting the time applies per window.  When over the
" limit syntax highlighting is disabled until |CTRL-L| is used.
" This is used to avoid that Vim hangs when using a very complicated
" pattern. Default: 2000.

" If this many milliseconds nothing is typed the swap file will be
" written to disk (see |crash-recovery|).  Also used for the
" |CursorHold| autocommand event.
"
" Advice: Reduce updatetime from 4000 to 300 to avoid issues with coc.nvim
" set updatetime=8000

" regex engine
" set regexpengine=2


 " URL: https://vim.fandom.com/wiki/Fix_meta-keys_that_break_out_of_Insert_mode
" On Unix-based systems, the Meta (Alt) key may not work in Vim. For example,
" in insert mode, pressing Meta-A (Alt-A) may exit to normal mode, then
" execute normal-mode commands. This can occur with non-GUI Vim under some
" terminal emulators – those which generate escape sequences for
" Meta-characters.
" The actually generated escape-sequences are <Esc>a .. <Esc>z.
function! FixMetaKeysTerminal() abort
  if !has('unix') || has('gui_running')
    return
  endif

  " In rxvt, the ':set <M-a>=^]a' commands do notwork. Instead only the map
  " commands are needed.
  for i in range(65, 90) + range(97, 122)
    let l:char = nr2char(i)
    exec "map \e" . l:char . ' <M-' . l:char . '>'
    exec "map! \e" . l:char . ' <M-' . l:char . '>'
  endfor
endfunction

" fix meta-keys which generate <Esc>a .. <Esc>z
function! FixMetaKeysTerminal2() abort
  let c = 'a'
  while c <=# 'z'
    exec 'set <M-' . toupper(c) . ">=\e" . c
    exec "imap \e" . c . ' <M-' . toupper(c) . '>'
    exec "cmap \e" . c . ' <M-' . toupper(c) . '>'
    let c = nr2char(1 + char2nr(c))
  endwhile
endfunction

" call FixMetaKeysTerminal()
" call FixMetaKeysTerminal2()

" }}}

" vim:foldmethod=marker:syntax=vim
