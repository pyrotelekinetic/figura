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

        " always use with utf-8 encoding
set encoding=utf-8

        " do not highlight matching search patterns
set hlsearch!

        " fall back on desert colorscheme for things not defined below
colorscheme desert

" Syntax highlighting ------------------------------------------------------------------------------------------------------------------------------------------------

        " white black
highlight Normal ctermfg=7 ctermbg=0

        " white black
highlight Comment ctermfg=7 ctermbg=0

        " yellow
highlight Constant ctermfg=3

        " cyan
highlight Statement ctermfg=6

        " bright cyan
highlight Identifier ctermfg=6 cterm=bold

        " bright green
highlight Type ctermfg=2 cterm=bold

        " red
highlight Special ctermfg=1

        " green
highlight PreProc ctermfg=2

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

" Status line --------------------------------------------------------------------------------------------------------------------------------------------------------

        " make status line always visible
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

function! MyTabLine()
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
        let s .= '%{MyTabLabel(' . (i + 1) . ')}'
    endfor

            " after the last tab fill with TabLineFill and reset tab page number
    let s .= '%#TabLineFill#%T'

            " right-align the label to close the current tab page
""    if tabpagenr('$') > 1
""        let s .= '%=%#TabLine#%999Xclose'
""    endif

    return s
endfunction

function! MyTabLabel(n)
    let tab = " ["

    if buflisted(a:n) == 0
        let tab .= "Unlisted"

            " call tab 'No Name' if buffer is unnamed
    elseif bufname(tabpagebuflist(a:n)[tabpagewinnr(a:n) - 1]) == ""
        let tab .= "No Name"
            " call tab by buffer name
    else
        let tab .= bufname(tabpagebuflist(a:n)[tabpagewinnr(a:n) - 1])
    endif

    let tab .= " (" . tabpagewinnr(a:n, '$') . ")"

    let tab .= "]"

            " show tab separator character on every tab except the last one
    if a:n != tabpagenr("$")
        let tab .= "Þ"
        ""|" \"³" \"Û" \"Ý" \"Þ" \"º"
    endif

    return tab
endfunction

set tabline=%!MyTabLine()

        " make selected tab white
highlight TabLineSel ctermfg=0 ctermbg=7

        " make unselected tabs DarkGrey
highlight TabLine ctermfg=0 ctermbg=8

        " fill unused tab line space with DarkGrey
highlight TablineFill ctermfg=8

" Folding ------------------------------------------------------------------------------------------------------------------------------------------------------------

source ~/dotfiles/folding.vim

" Key mapping --------------------------------------------------------------------------------------------------------------------------------------------------------

        " show highlight group under cursor with <F10>
noremap <F10> :echo "highlight<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

        " enter file explorer in directory of current buffer
""noremap - :Explore<cr>

        " enter file explorer in current directory of shell
""noremap _ :Explore .<cr>

        " write with sudo 
cnoremap W w !sudo tee > /dev/null %

" Indent guides ------------------------------------------------------------------------------------------------------------------------------------------------------

set conceallevel=2 | set concealcursor=nvic | syntax match IndentGuide /\v    /ms=e conceal cchar=│ containedin=ALL

augroup indentguidetoggle
    autocmd!
    autocmd WinEnter,BufEnter * set conceallevel=2 | set concealcursor=nvic | syntax match IndentGuide /\v    /ms=e conceal cchar=│ containedin=ALL
augroup END

highlight Conceal ctermbg=0 ctermfg=2

" Line numbering -----------------------------------------------------------------------------------------------------------------------------------------------------

        " turn on line numbering on startup
set number
set relativenumber

        " use hybrid numbering on current window and absolute numbering on idle windows
augroup numbertoggle
    autocmd!
    autocmd WinEnter,FocusGained,InsertLeave * set relativenumber | set number
    autocmd WinLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

        " make line number column background white and foreground black
highlight LineNr ctermbg=7 ctermfg=0

" Binary mode --------------------------------------------------------------------------------------------------------------------------------------------------------

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
