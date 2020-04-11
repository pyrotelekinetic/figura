" *** Misc. ***

" hide buffers when they are abandoned
set hidden

" better command line completion
set wildmode=list:longest,longest:full

" turn on the 'visual bell' - which is much quieter than the 'audio blink'
set visualbell

" use system clipboard for copy/paste
set clipboard=unnamedplus

" spellcheck for US English
setlocal spelllang=en_us

" always use with utf-8 encoding
set encoding=utf-8

" do not highlight matching search patterns
set hlsearch!

" always show one line above or below cursor
set scrolloff=1

" *** Syntax highlighting ***

let g:arcadia_Twilight = 1
colorscheme arcadia

filetype plugin indent on
syntax on

let hs_highlight_delimiters = 1
let hs_highlight_boolean = 1
let hs_highlight_types = 1
let hs_highlight_more_types = 1

let g:rainbow_active = 1
let g:rainbow_conf = {
\ 'ctermfgs': ['lightmagenta', 'lightgreen', 'lightblue', 'blue', 'green', 'magenta', 'red', 'lightred', 'lightcyan', 'cyan', 'yellow', 'lightyellow']
\ }

" highlight trailing whitespace
highlight TrailingSpace ctermbg=red
autocmd InsertEnter * match TrailingSpace /\s\+\%#\@<!$/
autocmd InsertLeave * match TrailingSpace /\s\+$/

" *** Indent settings ***

" move through groups of four spaces like a single tab
set shiftwidth=2

" one tab is four spaces
set softtabstop=2

" automatically convert/expand tabs to spaces
set expandtab

" automatically indent new lines smartly
set autoindent
set smartindent

" *** Status line ***

set laststatus=2 " make status line always visible

set showmode " show the current mode

set statusline=\  " add one space as padding
set statusline+=%r " show readonly flag
set statusline+=%m " show modified flag
set statusline+=%F " show relative path to file without expanding '~'
set statusline+=\ \  " two space padding
set statusline+=[%{strftime(\"%H:%M\ %d-%m-%Y\",getftime(expand(\"%:p\")))}] " show last modified timestamp
set statusline+=%= " align everything else to the right
set statusline+=%v,%l " show position in buffer: virtual column, linenumber
set statusline+=\  " add one space as padding

" *** Key mapping ***

" show highlight group under cursor with <F10>
noremap <F10> :echo "highlight<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" write with sudo
cnoremap W w !sudo tee > /dev/null %

" trim trailing whitespace from current line
noremap <F4> :s/\s\+$//e<CR>

" *** Line numbering ***

" turn on hybrid line numbering on startup
set number
set relativenumber

" use hybrid numbering on current window and absolute numbering on idle windows
augroup numbertoggle
    autocmd!
    autocmd WinEnter,FocusGained,InsertLeave * set relativenumber | set number
    autocmd WinLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

" *** Binary mode ***

" edit binary files using `xxd`
" `vim -b`
augroup Binary
    autocmd!
    autocmd BufReadPre  *.bin let &bin=1
    autocmd BufReadPost *.bin if &bin | %!xxd
    autocmd BufReadPost *.bin set ft=xxd | endif
    autocmd BufWritePre *.bin if &bin | %!xxd -r
    autocmd BufWritePre *.bin endif
    autocmd BufWritePost *.bin if &bin | %!xxd
    autocmd BufWritePost *.bin set nomod | endif
augroup END
