" Set defaults for fallback ------------------------------------------------------------------------------------------------------------------------------------------

  " use Enhanced Vim defaults
ru! defaults.vim

  " fall back on desert colorscheme for things not defined below
colo desert

" Misc. --------------------------------------------------------------------------------------------------------------------------------------------------------------

  " hide buffers when they are abandoned
set hidden

  " better command line completion
set wildmode=list:longest,longest:full

  " turn on the 'visual bell' - which is much quieter than the 'audio blink'
set visualbell

  " use system clipboard for copy/paste
set clipboard=unnamed

  " spellcheck for US English
setlocal spelllang=en_us

  " highlight matching search patterns
""set hlsearch

" Syntax highlighting ------------------------------------------------------------------------------------------------------------------------------------------------

  " white black
highlight Normal ctermfg=15 ctermbg=0

  " grey black
highlight Comment ctermfg=7 ctermbg=0

  " yellow
highlight Constant ctermfg=3 cterm=bold

  " magenta
highlight Statement ctermfg=5

  " bright cyan
highlight Identifier ctermfg=6

  " green
highlight Type ctermfg=10

  " red
highlight Special ctermfg=4

  " cyan
highlight PreProc ctermfg=3

  " show matching brackets
set showmatch

" Indent settings ----------------------------------------------------------------------------------------------------------------------------------------------------

  " move through groups of four spaces like a single tab
set shiftwidth=4

  " one tab is four spaces
set softtabstop=4

  " automatically convert/expand tabs to spaces
set expandtab

  " automatically intent new lines smartly
set autoindent
set smartindent

" Line numbering -----------------------------------------------------------------------------------------------------------------------------------------------------

  " always have absolute line numbering on
set number

  " use hybrid numbering on current window and absolute numbering on idle windows
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

  " make line number column background darkgrey and foreground black
highlight LineNr ctermbg=8 ctermfg=0

" Status line --------------------------------------------------------------------------------------------------------------------------------------------------------

  " make status line two lines deep so status line is always visible
set laststatus=2

  " show the current mode
set showmode

  " add one space as padding
set statusline=\ 

  " show readonly flag
set statusline+=%r

  " show modified flag
set statusline+=%m

  " show relative path to file without expanding '~'
set statusline+=%F

  " show last modified timestamp
set statusline+=\ (%{strftime(\"%H:%M\ %d-%m-%Y\",getftime(expand(\"%:p\")))})

  " align everything else to the right
set statusline+=%=

  " show position in buffer: virtual column, linenumber
set statusline+=%v,%l

  " add one space as padding 
set statusline+=\ 

  " make status line background color black and foreground color white
highlight StatusLine ctermbg=0 ctermfg=7

" Tab line -----------------------------------------------------------------------------------------------------------------------------------------------------------

  " make selected tab white
highlight TabLineSel ctermfg=0 ctermbg=7

  " make unselected tabs darkgrey
highlight TabLine ctermfg=0 ctermbg=8

  " fill usused tab line space with darkgrey
highlight TablineFill ctermbg=8

  " SE help: https://superuser.com/questions/331272/vim-show-the-index-of-tabs-in-the-tabline

function MyTabLine()
    let s = ''
    for i in range(tabpagenr('$'))

            " select the highlighting
        if i + 1 == tabpagenr()
            let s .= '%#TabLineSel#'
        else
            let s .= '%#TabLine#'
        endif

            " set the tab page number (for mouse clicks)
        let s .= '%' . (i + 1) . 'T'

            " the label is made by MyTabLabel()
        let s .= ' %{MyTabLabel(' . (i + 1) . ')} '
    endfor

        " after the last tab fill with TabLineFill and reset tab page nr
    let s .= '%#TabLineFill#%T'

        " right-align the label to close the current tab page
    if tabpagenr('$') > 1
        let s .= '%=%#TabLine#%999Xclose'
    endif

    return s
endfunction

function MyTabLabel(n)
    "" let buflist = tabpagebuflist(a:n)
    "" let winnr = tabpagewinnr(a:n)
    "" bufname(buflist[winnr - 1])
    let tab = "["
        " call tab 'No Name' if buffer is unnamed
    if bufname(tabpagebuflist(a:n)[tabpagewinnr(a:n) - 1]) == ""
        let tab .= "No Name"
    elseif &buftype(a:n) == "help"
        let tab .= "Help"
    else
        let tab .= bufname(tabpagebuflist(a:n)[tabpagewinnr(a:n) - 1])
    endif

    let tab .= " (" . tabpagewinnr(a:n, '$') . ")"

    let tab .= "]"

    return tab
endfunction

set tabline=%!MyTabLine()

" Key mapping --------------------------------------------------------------------------------------------------------------------------------------------------------

  " show highlight group under cursor with <F10>
noremap <F10> :echo "highlight<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

  " enter file explorer in directory of current buffer
noremap - :Explore<cr>

  " enter file explorer in current directory of shell
noremap _ :Explore .<cr>

  " write with sudo
""cnoremap W w !sudo tee > /dev/null %
