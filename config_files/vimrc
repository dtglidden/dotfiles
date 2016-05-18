set background=dark
set nocompatible
syntax on
set hidden
set wildmenu
set showcmd
set hlsearch
set ignorecase
set smartcase
set autoindent
set ruler
set laststatus=2
set confirm
set mouse=a
set cmdheight=2
set number
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab
set noerrorbells
set novisualbell
set showmatch
set nowrap
filetype plugin indent on
set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%04l,%04v]
map <F1> <Esc>:nohl<CR>

highlight WhiteSpaceEOL ctermbg=darkgreen
match WhiteSpaceEOL /\s\+$/

if has("autocmd")
  autocmd BufWritePre * :%s/\s\+$//e
endif

set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'

