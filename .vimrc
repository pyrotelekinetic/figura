" Syntax highlighting
colo desert                                    " Fall back to desert colorscheme for things not defined below
hi Normal ctermfg=white
hi Comment ctermfg=grey ctermbg=black cterm=italic
hi Constant ctermfg=darkyellow cterm=bold
hi Statement ctermfg=magenta
hi Identifier ctermfg=cyan
hi Type ctermfg=green
hi Special ctermfg=red
set showmatch                                  " Show matching brackets

" Misc.
ru! defaults.vim                        " Use Enhanced Vim defaults
set mouse=                              " Reset the mouse setting from defaults
set hidden                              " Hide buffers when they are abandoned
set wildmode=list:longest,longest:full  " Better command line completion
set visualbell                          " Turn on the 'visual bell' - which is much quieter than the 'audio blink'
"set hlsearch                            " Highlight matching search patterns

" Indent settings
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab      " Automatically convert/expand tabs to spaces
set autoindent     " Automatically intent new lines

" Line numbering
set number                                " Enable line numbers
hi LineNr ctermbg=darkgrey ctermfg=black

" Status line
set laststatus=2                                                                " Make status line two lines deep so status line is always visable
set showmode                                                                    " Show the current mode
set statusline=\                                                                " Add one space as padding
set statusline+=%r                                                              " Show readonly flag
set statusline+=%m                                                              " Show modified flag
set statusline+=%F                                                              " Show relative path to file without expanding '~'
set statusline+=\ (%{strftime(\"%H:%M\ %d/%m/%Y\",getftime(expand(\"%:p\")))})  " Show last modified timestamp
set statusline+=%=                                                              " Align everything else to the right
set statusline+=%l,%v                                                           " Show position in buffer: linenumber, virtual column
set statusline+=\                                                               " Add one space as padding 
hi StatusLine ctermbg=white                                                     " Change status line color

" Tab line
hi TabLine ctermfg=black ctermbg=white        " Make unselected tabs white
hi TabLineSel ctermfg=black ctermbg=darkgrey  " Make selected tab darkgrey
hi TabLineFill ctermbg=grey                   " Fill usused tab line space with grey

" Key mapping
nnoremap W b   " 'W' should be 'w' backwards
nnoremap E ge  " 'E' should be 'e' backwards
