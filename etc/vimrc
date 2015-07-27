" Vim Setting for Zachary Zhou
set nocompatible

" OS(unix/win32) setting 
if has("win32") && has("gui_running")
  source $VIMRUNTIME/vimrc_example.vim
  source $VIMRUNTIME/mswin.vim
  behave mswin
endif

" coding system
set encoding=utf-8
set fileencodings=utf-8-bom,ucs-bom,utf-8,cp936,gb18030,ucs,big5

" coding for term
let &termencoding=&encoding

" Font setting
set guifont=Microsoft\ YaHei\ Mono:h11:cANSI
set guifontset=-*-Microsoft\ YaHei\ Mono-medium-r-normal--11-*-*-*-*-*-iso8859-1,fixed

" UI setting
set shortmess=atI
set gcr=n:block-blinkon0
set listchars=tab:\^\ ,trail:.,extends:>,precedes:<,eol:$

" No scrollbar
"set guioptions-=l
"set guioptions-=L
"set guioptions-=r
"set guioptions-=R

" No menu toolbar
"set guioptions-=m
"set guioptions-=T

" Color scheme setting
if has("gui_running")
  colo desert
endif

" Other setting"
set novisualbell
set nobackup
set noswapfile
syntax on
set confirm
set autoindent
set cindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set smarttab
" no sound
set vb t_vb=
set noeb

if has("gui_running")
  set number
  set lines=27 columns=108  
endif

set history=1000
set ignorecase
set hlsearch
set incsearch
set gdefault

" Status line setting
set statusline=%<%f%m%r\ %y\ (%{&fileformat})\ \ (ascii=\%b,hex=\%B)\ \ (%l,%c)\ %p%%\ \ %=[%{strftime(\"%H:%M,%d/%m/%Y\")}]
set laststatus=2
set cmdheight=1
set ruler

" Diff setting
set diffexpr=MyDiff()
function MyDiff()
  let opt = '-a --binary '
  if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
  if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
  let arg1 = v:fname_in
  if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
  let arg2 = v:fname_new
  if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
  let arg3 = v:fname_out
  if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
  let eq = ''
  if $VIMRUNTIME =~ ' '
    if &sh =~ '\<cmd'
      let cmd = '""' . $VIMRUNTIME . '\diff"'
      let eq = '"'
    else
      let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
    endif
  else
    let cmd = $VIMRUNTIME . '\diff'
  endif
  silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3 . eq
endfunction

