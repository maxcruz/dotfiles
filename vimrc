" File type plugins
filetype plugin on

" Enable indentation
filetype indent on

" Unix as standar file type
set ffs=unix,mac,dos

" Read when a file is changed from the outside
set autoread

" Save using sudo for handling permissions
command W w !sudo tee % > /dev/null

" Format JSON
 command! FormatJSON %!python -m json.tool

" Turn on wild menu (command line complenion)
set wildmenu

" Show current position
set ruler

" Size of the command bar
set cmdheight=2

" Margin to the left
set foldcolumn=1

" Normal backspace behavior
set backspace=eol,start,indent

" Smartcase when search
set smartcase

" Highlight search results
set hlsearch

" Incremental searching
set incsearch

" Simplify regulars expressions
set magic

" Show pair bracket when text indicator is over
set showmatch

" Enable syntax highlighting
syntax on

" Beautiful color scheme 
colorscheme desert

" Fix text color when search
autocmd ColorScheme * highlight Search ctermfg=Black

" Force dark background
set background=dark

" UTF-8 encoding
set encoding=utf8

" Don't create swap fies
set noswapfile

" Don't make backups
set nobackup

" Use spaces instead of tab
set expandtab

" Smart tabs
set smarttab

" When indent use 4 spaces
set shiftwidth=4

" Existing tab with 4 spaces
set tabstop=4

" Don't wrap long lines
set nowrap

" Paste toggle (fix indentation when paste)
set pastetoggle=<F12>

" Show status line
set laststatus=2

" Format status line
set statusline=%#MyBuffer#\ %n\ %*
set statusline+=%#MyMode#\ %{Mode()}\ %*
set statusline+=%#MyFile#\ [%f]\ %*
set statusline+=%#MyModifiers#\ %h%m%r\ %*
set statusline+=%#MySpace#%=%*
set statusline+=%#MyType#\ %{&ft}\ \|\ %{Encode()}\ \|\ %{&ff}\ %*
set statusline+=%#MyCharacter#\ %c:%b\ %*
set statusline+=%#MyLine#\ %l/%L\ %*
set statusline+=%#MyPercent#\ %p%%\ %*

" Highlight groups
hi MyBuffer ctermbg=240 ctermfg=250
hi MyMode ctermfg=235
hi MyFile ctermbg=240 ctermfg=015
hi MyModifiers ctermbg=102 ctermfg=250
hi MySpace ctermbg=102
hi MyType ctermbg=102 ctermfg=250
hi MyPercent ctermbg=102 ctermfg=250
hi MyCharacter ctermbg=240 ctermfg=250
hi MyLine ctermbg=242 ctermfg=015

" Jump to the last position when reopening a file
if has("autocmd")
   au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif
endif

" Modes map
let g:modes = { 'p': ['PASTE', 099], 'n': ['NORMAL', 111], 'i': ['INSERT', 115], 'R': ['REPLACE', 179], 'v': ['VISUAL', 222], 'V': ['V-LINE', 223], "\<C-v>": ['V-BLOCK', 224], 'c': ['COMMAND', 050], 's': ['SELECT', 228], 'S': ['S-LINE', 229], "\<C-s>": ['S-BLOCK', 230], 't': ['TERMINAL', 086] }

" Function to get the current mode  and set the color
function! Mode() abort
    let l:mode = &paste ? 'p' : mode()
    let l:value = get(g:modes, mode, '')
    exec printf('hi MyMode ctermbg=%s', value[1])
    return value[0]
endfunction

" Function to get file encoding
function! Encode() abort
    if &fenc != ''
        return &fenc
    else
        return &enc
    endif
endfunction
