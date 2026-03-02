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

" Customize the markdown plugin
" TODO: in the default vimrc?
set conceallevel=0

" Vim bundle {{{

" Don't parse modelines (search 'vim modeline vulnerability').
" set nomodeline

let g:vim_bundle_dir =
  \ substitute(fnamemodify('~/.vim_bundle', ':p'), '\v/+$', '', 'g')
let g:vim_projects_Dir = g:vim_bundle_dir . '/projects'
execute 'set packpath^=' . fnameescape('~/.vim_bundle/packpath')

function! VimBundleInit() abort
  "
  " Migrate old directories
  "
  let l:list_dirs = [
  \  {'set': 'viewdir',
  \   'old': fnamemodify('~/.view_vim', ':p'),
  \   'new': g:vim_bundle_dir . '/view'},
  \
  \  {'set': 'backupdir',
  \   'old': fnamemodify('~/.backup_vim', ':p'),
  \   'new': g:vim_bundle_dir . '/backup'},
  \
  \  {'set': 'directory',
  \  'old': fnamemodify('~/.swp_vim', ':p'),
  \  'new': g:vim_bundle_dir . '/swp'},
  \
  \  {'set': 'undodir',
  \  'old': fnamemodify('~/.undo_vim', ':p'),
  \  'new': g:vim_bundle_dir . '/undo'}
  \ ]

  for l:dir in [g:vim_bundle_dir, g:vim_bundle_dir . '/misc']
    if !isdirectory(l:dir)
      call mkdir(g:vim_bundle_dir . '/misc', 'p', 0700)
    endif
  endfor

  for l:dir in l:list_dirs
    if !isdirectory(l:dir['new'])
      call mkdir(l:dir['new'], 'p', 0700)
    endif

    if isdirectory(l:dir['old'])
      call vim#migrate_dir(l:dir['old'], l:dir['new'])
    endif

    execute 'set ' . l:dir['set'] . '=' . fnameescape(l:dir['new'])
  endfor

  set swapfile
endfunction

" Init
call VimBundleInit()

" }}}

" FZF and CtrlP {{{

if exists('g:loaded_ctrlp')
  let g:ctrlp_map = '<c-p>'
  let g:ctrlp_cmd = 'CtrlP .'
  let g:ctrlp_use_caching = 0
  let g:ctrlp_by_filename = 0
  let g:ctrlp_working_path_mode = 'c'
  let g:ctrlp_clear_cache_on_exit = 1
  let g:ctrlp_show_hidden = 1
endif

" }}}

" ------------------> end
finish
" Plugins config {{{

" let g:font_default_name = get(g:, 'font_default_name', "Iosevka Semibold")

" Shell / grep {{{

if has('win32')
  " URL: http://vim.wikia.com/wiki/Use_cygwin_shell

  " Makes bash open in the working directory
  let $CHERE_INVOKING=1

  " Default path for Cygwin 64-bit, change accordingly
  set shell=bash.exe

  " Without --login, Cygwin won't mount some directories such as /usr/bin/
  set shellcmdflag=--login\ -c

  " Default value is (, but bash needs "
  set shellxquote=\"

  " Paths will use / instead of \
  set shellslash
elseif has('unix')
  set shell=/bin/bash
  set shellcmdflag=-c
endif

" }}}

" Disable the Background Color Erase that messes with some color schemes
" https://sunaku.github.io/vim-256color-bce.html
" set t_ut=

" if has('gui_running')
"   if &t_Co < 256
"     set t_Co=256
"   endif
" else
"   set t_Co=16
" endif

" skip /usr/share/vim/vim80/defaults.vim, because everything is in my vimrc
" (even vim >= 8.0 defaults.vim commands)
let skip_defaults_vim=1

" Environment variables {{{

if has('unix')
  if !exists('$FZF_DEFAULT_COMMAND')
    let $FZF_DEFAULT_COMMAND =
      \ 'find -L -not \( -type d -a -name .git -prune \) -not \( -type d \)'
  endif

  if !exists('$FZF_CTRL_T_COMMAND')
    let $FZF_CTRL_T_COMMAND = $FZF_DEFAULT_COMMAND
  endif

  if !exists('$FZF_CTRL_T_COMMAND')
    let $FZF_DEFAULT_OPTS =
      \ '--cycle -i --multi --exact --bind alt-j:down,alt-k:up'
  endif

  if !exists('$PYTHONPYCACHEPREFIX')
    let $PYTHONPYCACHEPREFIX = $HOME . '/.cache/python_cache'
    if !isdirectory($PYTHONPYCACHEPREFIX)
      call mkdir($PYTHONPYCACHEPREFIX, 'p', 0755)
    endif
  endif

  if !exists('$MYPY_CACHE_DIR')
    let $MYPY_CACHE_DIR = $HOME . '/.cache/python_cache_mypy'
    if !isdirectory($MYPY_CACHE_DIR)
      call mkdir($MYPY_CACHE_DIR, 'p', 0755)
    endif
  endif

  if !exists('$PYTHON_EGG_CACHE')
    let $PYTHON_EGG_CACHE = $HOME . '/.cache/python_cache_egg'
    if !isdirectory($PYTHON_EGG_CACHE)
      call mkdir($PYTHON_EGG_CACHE, 'p', 0755)
    endif
  endif

  if has('gui_running') && !exists('$TERM')
    let $TERM = 'xterm-256color'
  endif
endif

" }}}

" Vim built-in plugins {{{

" Disable the scripts ~/.local/share/vim/vim82
let g:loaded_matchparen = 1
let loaded_logiPat = 1
let g:loaded_vimballPlugin = 1
let g:loaded_2html_plugin = 1
let loaded_rrhelper = 1
let g:loaded_getscriptPlugin = 1
let g:loaded_zipPlugin = 1
let g:loaded_tarPlugin = 1

" vim82/pack/dist/opt/dvorak/plugin/dvorak.vim
let g:loaded_dvorak_plugin = 1

" }}}


" Vim Markdown {{{

let g:vim_markdown_conceal = 1
let g:vim_markdown_no_default_key_mappings = 1
let g:vim_markdown_folding_disabled = 1

" Do not highlight any ```code```
" let g:vim_markdown_fenced_languages = []

" Disabled because buggy
" let g:vim_markdown_folding_disabled = 0

" let g:vim_markdown_folding_style_pythonic = 1

" let g:vim_markdown_conceal_code_blocks = 0

" let g:vim_markdown_math = 1
" let g:vim_markdown_toc_autofit = 0
" let g:vim_markdown_strikethrough = 1
" let g:vim_markdown_auto_insert_bullets = 0
" let g:vim_markdown_new_list_item_indent = 0
"
" let g:vim_markdown_conceal = 1
"
" let g:vim_markdown_folding_level = 6
" let g:vim_markdown_folding_style_pythonic = 1
" "let g:vim_markdown_override_foldtext = 0

" }}}

" mkdx {{{

" Markdown

let g:mkdx#settings     = {
  \ 'highlight': {'enable': 1},
  \ 'enter': {'shift': 1},
  \ 'toc': {'text': 'Table of Contents', 'update_on_write': 0},
  \ 'fold': {'enable': 0},
  \ 'checkbox': {},
  \ 'auto_update': {}
  \ }

" Defines the list of states to use when toggling a checkbox. It can be set to
" a list of your choosing. Special characters must be escaped! Also, the list
" of toggles must contain at the very least, 2 items!
let g:mkdx#settings.checkbox.toggles = [' ', 'x']

" With this setting on, checkboxes that are toggled within checklists (lists
" of checkboxes) cause parent and child list items to be updated
" automatically. The states from g:mkdx#settings.checkbox.toggles are used to
" check and update the statusses of any parents. Children are force updated to
" the same token of their parent. To disable this behaviour entirely, set this
" value to 0. If you do not want children to be updated, set this value to 1
" instead.
let g:mkdx#settings.checkbox.update_tree = 2

" When enabled, changing certain settings in `g:mkdx#settings` will cause the
" document to be updated instantly. This is done using |dictwatcheradd| by
" checking if `has('*dictwatcheradd')` is true.
let g:mkdx#settings.auto_update.enable = 0

 " for vim-polyglot users, it loads Plasticboy's markdown
 " plugin which unfortunately interferes with mkdx list indentation.
let g:polyglot_disabled = ['markdown']

" }}}

" vim org {{{

let g:org_indent = 1

" }}}

" indentLine {{{

let g:indentLine_enabled = 1
let g:indentLine_fileType = ['python']
let g:indentLine_char = '┊'
" let g:indentLine_color_gui = '#A4E57E'
let g:indentLine_color_gui = '#CCCCCC'

" }}}

" python-mode {{{

let g:pymode = 1
let g:pymode_warnings = 0
let g:pymode_trim_whitespaces = 0
let g:pymode_options = 0
let g:pymode_python = 'python3'
let g:pymode_indent = 1
let g:pymode_folding = 0
let g:pymode_doc = 0
let g:pymode_virtualenv = 1
let g:pymode_run = 0
let g:pymode_breakpoint = 0  " C-b to add a breakpoint
let g:pymode_lint = 0
let g:pymode_rope = 0
let g:pymode_rope_completion = 0
let g:pymode_syntax = 1
" let g:pymode_virtualenv_path = $VIRTUAL_ENV

" }}}

" hjklmode {{{

let g:hjklmode_enabled = 0

" }}}

" Verdin {{{

" Compatibility with YouCompleteMe
let g:Verdin#disable_var_fragment = 1
let g:Verdin#fuzzymatch = 0
let g:Verdin#setomnifunc = 1

" If you use this plugin with another auto-complete plugin, such as
" YouCompleteMe <http://valloric.github.io/YouCompleteMe/>, set
" |g:Verdin#cooperativemode| as true.
" let g:Verdin#cooperativemode = 1

" default: 200
let g:Verdin#autocompletedelay = 100

" let g:Verdin#autoparen = 2

" local mapping for vim because it isn't supported by youcompleteme
" autocmd FileType vim inoremap <buffer> <C-Space> <C-x><C-o>
" autocmd FileType vim
"   \ imap <buffer> <expr> <C-Space> pumvisible() ? '<C-n>' : '<C-x><C-o>'

" }}}

" ultisnips {{{

" let g:UltiSnipsExpandTrigger = '<C-i>'
"let g:UltiSnipsExpandTrigger = '<tab>'

let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<S-tab>'

" Does not work well because it completes sometimes instead of jumping forward
" let g:UltiSnipsJumpForwardTrigger = '<C-i>'
" let g:UltiSnipsJumpBackwardTrigger = '<C-S-i>'
" }}}

" youcompleteme {{{

let g:ycm_auto_trigger = 0

let g:ycm_key_list_select_completion = ['<C-j>']
let g:ycm_key_list_previous_completion = ['<C-k>']

let g:ycm_min_num_of_chars_for_completion = 1

let g:ycm_use_ultisnips_completer = 0
let g:ycm_disable_for_files_larger_than_kb = 1500

" }}}

" deoplete {{{

let g:deoplete#enable_at_startup = 1
let g:python3_host_prog = exepath('python3')

" }}}

" }}}

" Vim Plugins Options {{{

" Vim yaml syntax (built-in) {{{

let g:yaml_recommended_style = 0
let g:yaml_schema = 'pyyaml'

" }}}

" Git Gutter {{{

let g:gitgutter_enabled = 0

" }}}

" UltiSnips {{{

let g:UltiSnipsUsePythonVersion = 3

" }}}

" Snipmate {{{

let g:snipMate = {'snippet_version' : 1}
" let g:snipMate.override = 1

" }}}

" Vim Template {{{

let g:templates_directory = ['~/.vim/templates/']

function! GetTemplateURL() abort
  return exists('g:template_url') ? g:template_url : ''
endfunction

let g:templates_user_variables = [
\   ['URL', 'GetTemplateURL'],
\ ]

" }}}

" NERD tree {{{

let NERDTreeCustomOpenArgs =
  \ {'file':  {'reuse': 'currenttab', 'where': 'p', 'keepopen': 1, 'stay': 1}}
let NERDTreeAutoDeleteBuffer = 0
let NERDTreeDirArrows = 1
let NERDTreeQuitOnOpen = 0  " Close NERDtree when the file is open
let NERDTreeRespectWildIgnore = 1
let NERDTreeShowBookmarks = 0
let NERDTreeShowHidden = 1
let NERDTreeShowLineNumbers = 1
let NERDTreeWinPos = 'right'
let NERDTreeWinSize = 30
let NERDTreeChDirMode = 2

" }}}

" ALE {{{

if filereadable(fnamemodify('~/.ansible-lint_global.yml', 'p'))
    \ && filereadable(fnamemodify('~/.bin/wrappers/ansible-lint-wrapper', ':p'))
  let g:ale_ansible_ansible_lint_executable =
    \ fnamemodify('~/.bin/wrappers/ansible-lint-wrapper', ':p')
endif

if filereadable(fnamemodify('~/.yamllint_global.yml', ':p'))
  let g:ale_yaml_yamllint_options =
    \ '-f parsable -c ' .
    \ shellescape(fnamemodify('~/.yamllint_global.yml', ':p'))
endif

" let g:ale_command_wrapper = '/usr/bin/env -i'
" let g:ale_command_wrapper = 'nice -n 19 chrt -i 0 ionice -c3'
" \    'python': g:custom_ale_fixers + ['isort', 'black'],


" if filereadable(fnamemodify('~/.bin/wrappers/vint-wrapper', ':p'))
"   let g:ale_vim_vint_executable =
"     \ fnamemodify('~/.bin/wrappers/vint-wrapper', ':p')
" endif

 let g:ale_sh_bashate_options = '-i E003,E006'

let g:ale_virtualtext_cursor = 0
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_enter = 0
let g:ale_enabled = 1
let g:ale_virtualenv_dir_names = []
let g:ale_python_prospector_options = '--rcfile ~/.pylintrc'
let g:ale_python_flake8_options = '-m flake8 --disable --ignore=D103'

let g:ale_linters_explicit = 1
let g:ale_echo_msg_format='%linter%: %code: %%s'

" 'cspell', 'pyflakes', 'pylama', 'pyright', 'unimport']
" 'pycodestyle',
" 'pydocstyle', 'bandit', 'pyre',
" 'vulture', 'pyre',
"
"'flake8',
"
" 'vulture'
" recently removed: 'pyflakes'
"
let g:ale_linters = {'python': ['pylint', 'pycodestyle', 'mypy'],
\                    'sh': ['shell', 'shellcheck', 'bashate'],
\                    'vim': ['ale_custom_linting_rules', 'vint'],
\                    'yaml.ansible': ['yamllint', 'ansible_lint'],
\                    'yaml': ['yamllint']}

let g:ale_lint_delay = 500
" let g:ale_shell = '/bin/sh'
" let g:languagetool_enable_categories = 'PUNCTUATION,TYPOGRAPHY,CASING,' .
"   \ 'COLLOCATIONS,CONFUSED_WORDS,CREATIVE_WRITING,GRAMMAR,MISC,' .
"   \ 'MISUSED_TERMS_EU_PUBLICATIONS,NONSTANDARD_PHRASES,PLAIN_ENGLISH,TYPOS,'
"   \ 'REDUNDANCY,SEMANTICS,TEXT_ANALYSIS,STYLE,GENDER_NEUTRALITY'
" let g:languagetool_enable_rules = 'AND_ALSO,ARE_ABLE_TO,ARTICLE_MISSING'
"   \ ',AS_FAR_AS_X_IS_CONCERNED,BEST_EVER,BLEND_TOGETHER,BRIEF_MOMENT,'
"   \ 'CAN_NOT,CANT_HELP_BUT,COMMA_WHICH,EG_NO_COMMA,ELLIPSIS,EXACT_SAME,'
"   \ 'HONEST_TRUTH,HOPEFULLY,IE_NO_COMMA,IN_ORDER_TO,I_VE_A,NEGATE_MEANING,'
"   \ 'PASSIVE_VOICE,PLAN_ENGLISH,REASON_WHY,SENT_START_NUM,SERIAL_COMMA_OFF,'
"   \ 'SERIAL_COMMA_ON,SMARTPHONE,THREE_NN,TIRED_INTENSIFIERS,ULESLESS_THAT,'
"   \ 'WIKIPEDIA,WORLD_AROUND_IT'
" let g:ale_pattern_options_enabled = 1

" -d Disabled rules (Comma separated)
" DASH_RULE: lines that start with a "-" (md)
let g:ale_languagetool_options = '-d DASH_RULE'

" ale
" let g:custom_ale_fixers = ['remove_trailing_lines', 'trim_whitespace']
" let g:ale_pattern_options =
"   \ {'^.*ansible.*.yml$': {'ale_linters': ['yamllint', 'ansible-lint'],
"   \                        'ale_fixers': g:custom_ale_fixers}}
" \                   'python': g:custom_ale_fixers + ['isort', 'black'],
let g:ale_fixers = {'python': ['isort']}
let g:ale_python_autopep8_options = ''

" augroup AleFixersCustom
"   autocmd!
"   autocmd BufWritePost *.py
"     \ if exists('b:delete_white_space') && exists(':ALEFix') | ALEFix | endif
" augroup END

" }}}


" MRU {{{

let MRU_File = g:vim_bundle_dir . '/misc/vim_mru_files'
let g:MRU_Max_Entries = 800
let g:MRU_Exclude_Files = '^/tmp/.*\|^/var/tmp/.*'
let g:MRU_Window_Height = 15

" }}}

" vim-jedi {{{

let g:jedipydoc_way_to_show_pydoc = 'tab'
let g:jedipydoc_documentation_command = 'K'
let g:jedi#documentation_command = '<Leader>k'

let g:jedi#rename_command='<Leader>R'
let g:jedi#rename_command_keep_name ='<Leader>r'
let g:jedi#case_insensitive_completion = 0
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 0
let g:jedi#auto_close_doc = 1
let g:jedi#show_call_signatures = 1    " help by showing the arguments

" <Leader>d will split, instead of go to in the same buffer
" let g:jedi#use_splits_not_buffers = 1

" }}}

" Buf Explorer {{{

" let g:bufExplorerSortBy='name'       " Sort by the buffer's name.
let g:bufExplorerShowDirectories=0   " Do not show directories.

" }}}

" localvimrc {{{

" localvimrc
let g:localvimrc_persistent = 1
let g:localvimrc_persistence_file =
  \ g:vim_bundle_dir . '/misc/localvimrc_persistent'
let g:localvimrc_sandbox = 0

" }}}

" vim polyglot {{{

" let g:polyglot_disabled = ['vim']
let g:polyglot_disabled = ['autoindent', 'sensible']

let g:python_highlight_space_errors = 0
let g:python_highlight_class_vars = 1
let g:python_highlight_builtins = 1
let g:python_highlight_func_calls = 0

" }}}

" Molokai color scheme {{{

let g:molokai_original = 1
let g:rehash256 = 1

" }}}

" Color scheme: PaperColor {{{

let g:PaperColor_Theme_Options = {
  \   'theme': {
  \     'default.light': {
  \       'override' : {
  \         'folded_bg' : ['#dddddd', '232']
  \       }
  \     }
  \   }
  \ }

" }}}

" Vim Easy Session {{{

let g:easysession_dir = g:vim_bundle_dir . '/sessions'

let g:easysession_default_session = 'main'

let g:easysession_auto_save = 0  " Saved by vimrc

let g:easysession_save_guifont = 1
let g:easysession_save_colorscheme = 1

if 0 || !exists('$VIM_PERSISTENT_SESSION_ENABLED')
      \ || $VIM_PERSISTENT_SESSION_ENABLED ==# 0
  let s:session_enabled = 0
  let g:easysession_register_autocmd = 0
  let g:easysession_auto_load = 0
  let g:easysession_auto_save = 0
else
  let s:session_enabled = 1
  let g:easysession_auto_load = 1
endif
let $VIM_PERSISTENT_SESSION_ENABLED=0

" End vim easy session }}}

" Dirvish git {{{

let g:dirvish_git_indicators = {
  \ 'Modified'  : '✹',
  \ 'Staged'    : '✚',
  \ 'Untracked' : '✭',
  \ 'Renamed'   : '➜',
  \ 'Unmerged'  : '═',
  \ 'Ignored'   : '☒',
  \ 'Unknown'   : '?'
  \ }

" Used for <Plug>(dovish_move)
if exepath('smartmv')
  function! g:DovishMove(target, destination) abort
    return 'smartmv ' . a:target . ' ' . a:destination
  endfunction
endif

if exepath('trash-put')
  " Used for <Plug>(dovish_delete)
  function! g:DovishDelete(target) abort
    return 'trash-put ' . a:target
  endfunction
endif

" }}}

" }}}
" Functions {{{

function! Fasd(query)
  " -d: Match directories only.
  " -l: List paths without scores.
  let l:cmd = 'fasd -d -l ' . shellescape(a:query)
  let l:result = systemlist(l:cmd)
  if v:shell_error !=# 0
    echoerr 'Fasd error: ' . join(l:result, "\n")
    return
  endif

  if len(l:result) ==# 0 || !isdirectory(l:result[0])
    echo 'Fasd has not found anything for: ' . a:query
    return
  endif

  let l:path = l:result[0]
  exec 'edit ' . fnameescape(l:path)
  exec 'lcd ' . fnameescape(l:path)
  echo l:path
endfunction

command! -bar -nargs=1 Fasd call Fasd(<f-args>)

function! s:fasd_input() abort
  let l:name = trim(input('Fasd query: '))
  if empty(l:name)
    return
  endif

  silent call Fasd(l:name)
endfunction

" The 'patch-8.2.1978' adds the '<cmd>' feature
if has('patch-8.2.1978')
  nnoremap ,fd <cmd>call <SID>fasd_input()<CR>
else
  nnoremap ,fd :call <SID>fasd_input()<CR>
endif

function! AllAbbreviations() abort
  return

  iabclear

  " if exists('*AutoCorrect')
  "   call AutoCorrect()
  " endif

  if !exists(':Abolish')
    return
  endif

  Abolish hte the
  Abolish tno the
  Abolish ot to
  Abolish buton{,s} button{}
  Abolish builtin built-in
  Abolish thirdparty third-party
  Abolish commandline command-line
  Abolish mathod{,s} method{}
  Abolish layouit{,s} layout{}
  Abolish oif of
  Abolish obselete obsolete
  Abolish argumetn{,s} argument{}
endfunction

call AllAbbreviations()

" }}}

" Init {{{

" Arch Linux set runtimepath
set runtimepath-=/usr/share/vim/vimfiles
set runtimepath-=/usr/share/vim/vimfiles/after

" Other operating systems
set runtimepath-=/etc/vim
set runtimepath-=/etc/vim/after
set runtimepath-=/var/lib/vim/addons
set runtimepath-=/var/lib/vim/addons/after

if has('win32')
  set runtimepath+=~/.vim
endif

function! s:prepend_runtimepath(path) abort
  let &runtimepath = a:path . ((empty(a:path) ? '' : ',')) . &runtimepath
endfunction

if isdirectory(fnamemodify('~/.vim.local', ':p'))
  call s:prepend_runtimepath('~/.vim.local')
endif

" TODO: remove
function! GetCwdWhenDifferentFromDirname() abort
  if &filetype ==# 'netrw'
    return
  endif

  let l:cwd = getcwd()
  if l:cwd !=# expand('%:p:h')
    return l:cwd
  endif
  return ''
endfunction

function! GetSession() abort
  if exists('*easysession#load')
    return printf('[%s]', easysession#name())
  endif

  return ''
endfunction
" }}}

" Aliases and abbreviations {{{

" call vim#cmd_alias('Write', 'w')
" call vim#cmd_alias('TabRename', 'tabrename')
" call vim#cmd_alias('Rename', 'rename')
" call vim#cmd_alias('Mkdir', 'mkdir')
" call vim#cmd_alias('XdgOpen', 'xo')
" call vim#cmd_alias('Rename', 'ren')
" call vim#cmd_alias('Remove', 'rm')
" call vim#cmd_alias('Move', 'move')
" call vim#cmd_alias('Move', 'mv')
" call vim#cmd_alias('Copy', 'cp')
" call vim#cmd_alias('Chmod', 'chmod')
" call vim#cmd_alias('', '')
" call vim#cmd_alias('', '')
" call vim#cmd_alias('E', 'e')
" call vim#cmd_alias('EC', 'ec')
" call vim#cmd_alias('CI', 'ci')
" call vim#cmd_alias('Which', 'which')
" call vim#cmd_alias('Diff', 'diff')
" call vim#cmd_alias('Reload', 'reload')
" call vim#cmd_alias('Reload', 'cls')
" call vim#cmd_alias('CIP', 'cip')
" call vim#cmd_alias('GPL', 'gpl')
" call vim#cmd_alias('Grep', 'grep')
" call vim#cmd_alias('FGrep', 'fgrep')
" call vim#cmd_alias('Source %', 'so')
" call vim#cmd_alias('Source', 'source')
" call vim#cmd_alias('Dict', 'dict')
" call vim#cmd_alias('bel vert Git', 'git')
" call vim#cmd_alias('bel vert Git log -p %', 'glp')
" call vim#cmd_alias('bel vert Git diff %', 'gd')
" call vim#cmd_alias('edit ++enc=latin1', 'latin1')
" call vim#cmd_alias('edit ++enc=latin1', 'latin')
" call vim#cmd_alias('edit ++enc=latin1', 'ascii')

" }}}


" Spell Checking {{{
function! AddSpellExceptions() abort
  return
  " Avoid spell checking URI
  " syntax match DoNotSpellUri /\v[a-zA-Z]+:\/\/[^[:space:]]+/ transparent
  "   \ contains=@DoNotSpell
  " syntax cluster Spell add=DoNotSpellUri

  " Avoid spell checking both CamelCase formatted identifiers and
  " uppercase identifiers. Since most languages (excluding Raku) prohibit
  " Unicode in identifiers, these matches are intentionally confined to ASCII
  " codepoints (e.g., "[A-Z]" rather than "[[:upper:]]").
  " syntax match DoNotSpellCaps
  "   \ /\v<[A-Za-z]([A-Z0-9]{-1,}|[a-z0-9]+[A-Z0-9].{-})>/
  "   \ transparent contains=@DoNotSpell
  " syntax match DoNotSpellCaps /\v<[A-Z]([A-Za-z0-9]+)>/
  "   \ transparent contains=@DoNotSpell
  " syntax match DoNotSpellCaps
  "   \ /\v<[A-Za-z]([A-Z0-9]{-1,}|[a-z0-9]+[A-Z0-9].{-})>/
  "   \ transparent contains=@DoNotSpell
  syntax match DoNotSpellCaps /\v<\u\w+\u\w+>/ transparent contains=@DoNotSpell
  syntax cluster Spell add=DoNotSpellCaps

  " Paths like papa/thth/toto/dirdir
  " syntax match DoNotSpellPaths '\v([^\/]+\/)(\/\s|[^\/]+)(\s|$)'
  "   \ transparent contains=@DoNotSpell
  " syntax cluster Spell add=DoNotSpellPaths

  " Avoid spell checking snake_case formatted identifiers.
  " syntax match DoNotSpellPythonSnake /\v<\w+_.{-1,}>/
  "   \ transparent contains=@DoNotSpell
  syntax match DoNotSpellPythonSnake /\<[a-z]\+_[a-z_]\+\>/
    \ transparent contains=@DoNotSpell
  syntax cluster Spell add=DoNotSpellPythonSnake

  " if &filetype ==# 'vim'
  "   " Do not spell check: set var
  "   syntax match DoNotSpellVimSet
  "     \ /\v set [a-z]+($|[\-\+]{0,1}\=[^[:space:]]+)/
  "     \ transparent contains=@DoNotSpell
  "   syntax cluster Spell add=DoNotSpellVimSet

  "   syntax match DoNotSpellAutocmd /\v"\s+autocmd.*$/ transparent
  "     \ contains=@DoNotSpell
  "   syntax cluster Spell add=DoNotSpellAutocmd

  "   syntax match DoNotSpellIf /\v"\s+if.*$/ transparent contains=@DoNotSpell
  "   syntax cluster Spell add=DoNotSpellIf
  " endif

  " if &filetype ==# 'python'
  "   " Avoid spell checking "@"-prefixed identifiers.
  "   syntax match DoNotSpellPythonDecorator /\v\@[a-zA-Z].{-}>/
  "     \ transparent contains=@DoNotSpell
  "   syntax cluster Spell add=DoNotSpellPythonDecorator

  "   " Avoid spell checking ":"-delimited substrings.
  "   syntax match DoNotSpellPythonColons /\v:[^:]+:/ transparent
  "     \ contains=@DoNotSpell
  "   syntax cluster Spell add=DoNotSpellPythonColons

  "   " highlight def link DoNotSpellPythonColons Comment
  "   " Avoid spell checking "`"-delimited substrings.
  "   syntax match DoNotSpellPythonTicks /\v`[^`]+`/ transparent
  "     \ contains=@DoNotSpell
  "   syntax cluster Spell add=DoNotSpellPythonTicks
  " endif

  syntax match DoNotSpellBrackets /\v\[[^\[\]]+\]/ transparent contains=@DoNotSpell
  syntax cluster Spell add=DoNotSpellBrackets

  syntax match DoNotSpellQuoted /\v\'[^\']+\'/ transparent contains=@DoNotSpell
  syntax cluster Spell add=DoNotSpellQuoted

  if &filetype !=# 'vim'
    syntax match DoNotSpellDoubleQuoted /\v\"[^\"]+\"/ transparent contains=@DoNotSpell
    syntax cluster Spell add=DoNotSpellDoubleQuoted
  endif

  " Avoid spell checking '"'-delimited filetyped filenames matched as a
  " " double-quoted substring containing a filename prefix, a period, and one to
  " " four characters comprising a file type.
  " syntax match NoSpellPythonPath /\v"[^"]+.[^"]{1,4}"/
  "   \ transparent contained containedin=pythonComment,python.*String
  "   \ contains=@NoSpell
endfunction

" autocmd TabEnter * wincmd =

" For the most accurate but slowest result, set the syntax synchronization
" method to fromstart. This can be done with an autocmd in your vimrc:
" https://vim.fandom.com/wiki/Fix_syntax_highlighting
" autocmd BufWinEnter,Syntax * syntax sync minlines=500 maxlines=500

" autocmd FileType sh setlocal autoindent
" autocmd BufRead,BufNewFile *.j2 setfiletype jinja2
" autocmd BufRead,BufNewFile *.yaml setfiletype yaml
" autocmd BufRead,BufNewFile *.yml setfiletype yaml
" listchars=tab:>-,trail:-,extends:>,precedes:<,nbsp:+
" autocmd FileType Dict:* set syntax=stardict
" listchars=tab:>-,trail:-,extends:>,precedes:<,nbsp:+
" autocmd FileType sh,vim,python,text,yaml,yaml.ansible

" Close NERD Tree
" autocmd WinEnter * if exists("g:NERDTree") && exists('t:NERDTreeBufName')
"   \ && bufwinnr(t:NERDTreeBufName) != -1 && winnr('$') == 1 | quit | endif
" File types
" autocmd BufNewFile,BufRead * call AddSpellExceptions() | setlocal spell
" \ spelllang=en

" autocmd FileType vim,python,text,yaml,yaml.ansible setlocal
"  \ foldmethod=expr foldexpr=folding#foldexpr_indent(v:lnum)
" autocmd FileType text,python,sh,puppet,json,ruby,yaml,yaml.ansible,vim
"       \ setlocal number list listchars=tab:>-,extends:>,precedes:<,nbsp:+

" Alternative highlight tabs
" highlight UselessChars ctermbg=red guibg=red
" autocmd BufWinEnter * match UselessChars /\v[\t\s]\+/

" autocmd BufWinLeave * call clearmatches()

" function! s:session_load() abort
"   wincmd =
"   silent! argdelete
" endfunction
" autocmd SessionLoadPost * call <SID>session_load()

" Fix vim-yaml syntax issue when a session is loaded
" autocmd Syntax *.yaml,*.yml syntax on

" function! s:VimEnter()
  " autocmd SessionLoadPost * if exists('g:easysession_save_argument_list')
  " \ && !g:easysession_save_argument_list | silent! argdelete * | endif

  " Load the color scheme
" endfunction

" autocmd VimEnter * nested call <SID>VimEnter()

" There is a bug. It closes buffers that are part of :grep.
" autocmd WinClosed * silent call buffer#close_hidden()

" autocmd BufReadPost * let b:git_branch = GitBranch()
" autocmd FileType vim nnoremap <buffer> <A-K> :call AnyHelp(expand('<cword>'))<CR>

" augroup XdgOpen
"    autocmd!
"    " autocmd BufReadCmd *.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz,*.tar.zst,*.7z,*.jpg,*.bmp,*.gif,*.webp,*.png,*.jpeg silent call XdgOpen(expand('<afile>'))
"    autocmd BufReadCmd *.zip,*.rar,*.tar.*,*.7z,*.jpg,*.jpeg,*.bmp,*.gif,*.webp,*.png silent call unix#xdg_open(expand('<afile>'), 1)
" augroup end

" }}}

" if s:session_enabled
"   function! s:custom_easysession_save() abort
"     if exists(':Dirvish')
"       let l:dirvish_buffers = []
"       for l:buf in getbufinfo()
"         if getbufvar(l:buf.bufnr, '&filetype') ==# 'dirvish'
"           call add(l:dirvish_buffers, {
"             \ 'bufnr': l:buf.bufnr,
"             \ 'buftype': getbufvar(l:buf.bufnr, '&buftype')
"           \ })
"           call setbufvar(l:buf.bufnr, '&buftype', '')
"         endif
"       endfor
"     endif
"
"     if exists('*easysession#load')
"       try
"         call easysession#save()
"       finally
"         if exists(':Dirvish')
"           for l:buf in l:dirvish_buffers
"             call setbufvar(l:buf.bufnr, '&buftype', l:buf.buftype)
"           endfor
"         endif
"       endtry
"     endif
"   endfunction
"
"   augroup EasySessionCustom
"     autocmd!
"     " autocmd VimEnter * nested :call easysession#load(easysession#name(), 1)
"     autocmd VimLeavePre *
"       \ if !v:dying | :call <SID>custom_easysession_save() | endif
"     autocmd BufWritePost * :call <SID>custom_easysession_save()
"   augroup END
" endif

" vim:foldmethod=marker:syntax=vim
