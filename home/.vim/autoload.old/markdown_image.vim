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

" markdown_image.vim
" Requires: string.vim

scriptencoding utf-8

function! s:safe_make_dir() abort
  if s:os ==# 'Windows'
    let outdir = expand('%:p:h') . '\' . g:markdown_image_imgdir . '\' . expand('%:t')
  else
    let outdir = expand('%:p:h') . '/' . g:markdown_image_imgdir . '/' . expand('%:t')
  endif
  if !isdirectory(outdir)
    call mkdir(outdir, 'p')
  endif
  return fnameescape(outdir)
endfunction

function! s:save_file_tmp_linux(imgdir, tmpname) abort
  let targets = filter(
        \ systemlist('xclip -selection clipboard -t TARGETS -o'),
        \ 'v:val =~# ''image/''')
  if empty(targets) | return 1 | endif

  let mimetype = targets[0]
  let extension = split(mimetype, '/')[-1]
  let tmpfile = a:imgdir . '/' . a:tmpname . '.' . extension
  call system(printf('xclip -selection clipboard -t %s -o > %s',
        \ mimetype, tmpfile))
  return tmpfile
endfunction

function! s:save_file_tmp_win32(imgdir, tmpname) abort
  let tmpfile = a:imgdir . '/' . a:tmpname . '.png'

  let clip_command = 'Add-Type -AssemblyName System.Windows.Forms;'
  let clip_command .= 'if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {'
  let clip_command .= "[System.Drawing.Bitmap][System.Windows.Forms.Clipboard]::GetDataObject().getimage().Save('"
  let clip_command .= tmpfile ."', [System.Drawing.Imaging.ImageFormat]::Png) }"
  let clip_command = "powershell -sta \"".clip_command. "\""

  silent call system(clip_command)
  if v:shell_error ==# 1
    return 1
  else
    return tmpfile
  endif
endfunction

function! s:save_file_tmp_macos(imgdir, tmpname) abort
  let tmpfile = a:imgdir . '/' . a:tmpname . '.png'
  let clip_command = 'osascript'
  let clip_command .= ' -e "set png_data to the clipboard as «class PNGf»"'
  let clip_command .= ' -e "set referenceNumber to open for access POSIX path of'
  let clip_command .= ' (POSIX file \"' . tmpfile . '\") with write permission"'
  let clip_command .= ' -e "write png_data to referenceNumber"'

  silent call system(clip_command)
  if v:shell_error ==# 1
    return 1
  else
    return tmpfile
  endif
endfunction

function! s:save_file_tmp(imgdir, tmpname) abort
  if s:os ==# 'Darwin'
    return s:save_file_tmp_macos(a:imgdir, a:tmpname)
  elseif s:os ==# 'Linux'
    return s:save_file_tmp_linux(a:imgdir, a:tmpname)
  elseif s:os ==# 'Windows'
    return s:save_file_tmp_win32(a:imgdir, a:tmpname)
  endif
endfunction

function! s:save_new_file(imgdir, tmpfile) abort
  let extension = split(a:tmpfile, '\.')[-1]
  let reldir = g:markdown_image_imgdir . '/' . expand('%:t')
  let cnt = 0
  let filename = a:imgdir . '/' . g:markdown_image_imgname . cnt . '.' . extension
  let relpath = reldir . '/' . g:markdown_image_imgname . cnt . '.' . extension
  while filereadable(filename)
    call system('diff ' . a:tmpfile . ' ' . filename)
    if !v:shell_error
      call delete(a:tmpfile)
      return relpath
    endif
    let cnt += 1
    let filename = a:imgdir . '/' . g:markdown_image_imgname . cnt . '.' . extension
    let relpath = reldir . '/' . g:markdown_image_imgname . cnt . '.' . extension
  endwhile
  if filereadable(a:tmpfile)
    call rename(a:tmpfile, filename)
  endif
  return relpath
endfunction

function! s:random_name() abort
  let l:new_random = strftime('%Y%m%d%H%M')
  return l:new_random
endfunction

function! markdown_image#from_clipboard() abort
  " detect os: https://vi.stackexchange.com/questions/2572/detect-os-in-vimscript
  let s:os = 'Windows'
  if !(has('win64') || has('win32') || has('win16'))
    let s:os = substitute(system('uname'), "\n", '', '')
  endif

  let workdir = s:safe_make_dir()
  " change temp-file-name and image-name
  let g:markdown_image_tmpname = s:random_name()
  " let g:markdown_image_imgname = g:markdown_image_tmpname

  let tmpfile = s:save_file_tmp(workdir, g:markdown_image_tmpname)
  if tmpfile ==# 1
    return
  else
    " let relpath = s:save_new_file(g:markdown_image_imgdir, tmpfile)
    let extension = split(tmpfile, '\.')[-1]
    let relpath = g:markdown_image_imgdir . '/' . expand('%:t') . '/' . g:markdown_image_tmpname . '.' . extension
    execute 'normal! i![Image](' . relpath . ')'
  endif
endfunction

function! markdown_image#open_image() abort
  if ! has('unix')
    echo 'Error: only UNIX operating systems are supported.'
    return
  endif

  let line = getline('.')

  let prefix='![Image]('
  let suffix=')'

  " find ![Image](
  let index = virtcol('.') - 1
  let prefix_found = 0
  while index >= 0
    if line[index:index + len(prefix) - 1] ==# prefix
      let prefix_found = 1
      break
    endif
    let index = index - 1
  endwhile

  " find suffix
  let suffix_index = stridx(line, suffix, virtcol('.') - 1)

  if prefix_found ==# 0
    echo "Error: the string '" . prefix . "' was not found."
    return
  endif

  if suffix_index < 0
    echo "Error: '" . prefix . "' followed by a path to a filename and closed with '" . suffix . "'."
    return
  endif

  let path_to_edited_file = expand('%:p:h')

  let image_path = string#strip(line[index + len(prefix):suffix_index - 1])
  if filereadable(path_to_edited_file + '/' + image_path)
    echo 'Error: "' . image_path . '" does not exist.'
    return
  endif

  let cmd = 'cd ' . shellescape(path_to_edited_file, 1) . '; xdg-open ' . shellescape(image_path, 1)

  call system(cmd)
endfunction
