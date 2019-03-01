" Set defaults for fallback ------------------------------------------------------------------------------------------------------------------------------------------

        " use Enhanced Vim defaults
runtime! defaults.vim

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
highlight Comment ctermfg=7 ctermbg=0 cterm=italic

""highlight SpecialCommment ctermbg=3

        " yellow
highlight Constant ctermfg=6 cterm=bold

        " cyan
highlight Statement ctermfg=3

        " bright cyan
highlight Identifier ctermfg=11

        " bright green
highlight Type ctermfg=10

        " red
highlight Special ctermfg=4

        " green
highlight PreProc ctermfg=2

        " show matching brackets
set showmatch

""ctermfg={color-nr}				*highlight-ctermfg* *E421*
""ctermbg={color-nr}				*highlight-ctermbg*
""	The {color-nr} argument is a color number.  Its range is zero to
""	(not including) the number given by the termcap entry "Co".
""	The actual color with this number depends on the type of terminal
""	and its settings.  Sometimes the color also depends on the settings of
""	"cterm".  For example, on some systems "cterm=bold ctermfg=3" gives
""	another color, on others you just get color 3.
""
""	For an xterm this depends on your resources, and is a bit
""	unpredictable.	See your xterm documentation for the defaults.	The
""	colors for a color-xterm can be changed from the .Xdefaults file.
""	Unfortunately this means that it's not possible to get the same colors
""	for each user.	See |xterm-color| for info about color xterms.
""
""	The MSDOS standard colors are fixed (in a console window), so these
""	have been used for the names.  But the meaning of color names in X11
""	are fixed, so these color settings have been used, to make the
""	highlighting settings portable (complicated, isn't it?).  The
""	following names are recognized, with the color number used:
""
""							*cterm-colors*
""	    NR-16   NR-8    COLOR NAME ~
""	    0	    0	    Black
""	    1	    4	    DarkBlue
""	    2	    2	    DarkGreen
""	    3	    6	    DarkCyan
""	    4	    1	    DarkRed
""	    5	    5	    DarkMagenta
""	    6	    3	    Brown, DarkYellow
""	    7	    7	    LightGray, LightGrey, Gray, Grey
""	    8	    0*	    DarkGray, DarkGrey
""	    9	    4*	    Blue, LightBlue
""	    10	    2*	    Green, LightGreen
""	    11	    6*	    Cyan, LightCyan
""	    12	    1*	    Red, LightRed
""	    13	    5*	    Magenta, LightMagenta
""	    14	    3*	    Yellow, LightYellow
""	    15	    7*	    White
""
""	The number under "NR-16" is used for 16-color terminals ('t_Co'
""	greater than or equal to 16).  The number under "NR-8" is used for
""	8-color terminals ('t_Co' less than 16).  The '*' indicates that the
""	bold attribute is set for ctermfg.  In many 8-color terminals (e.g.,
""	"linux"), this causes the bright colors to appear.  This doesn't work
""	for background colors!	Without the '*' the bold attribute is removed.
""	If you want to set the bold attribute in a different way, put a
""	"cterm=" argument AFTER the "ctermfg=" or "ctermbg=" argument.	Or use
""	a number instead of a color name.
""
""	The case of the color names is ignored.
""	Note that for 16 color ansi style terminals (including xterms), the
""	numbers in the NR-8 column is used.  Here '*' means 'add 8' so that Blue
""	is 12, DarkGray is 8 etc.
""
""	Note that for some color terminals these names may result in the wrong
""	colors!
""
""	You can also use "NONE" to remove the color.

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
highlight StatusLine ctermbg=7 ctermfg=0

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

            " after the last tab fill with TabLineFill and reset tab page nr
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

    if a:n != tabpagenr("$")
        let tab .= "Þ"
        ""|" \"³" \"Û" \"Ý" \"Þ" \"º"
    endif

    return tab
endfunction

set tabline=%!MyTabLine()

        " make selected tab white
highlight TabLineSel ctermfg=0 ctermbg=7

        " make unselected tabs darkgrey
highlight TabLine ctermfg=0 ctermbg=8

        " fill usused tab line space with darkgrey
highlight TablineFill ctermbg=8

" Folding ------------------------------------------------------------------------------------------------------------------------------------------------------------

""source ~\_newfold.vim

" Key mapping --------------------------------------------------------------------------------------------------------------------------------------------------------

        " show highlight group under cursor with <F10>
noremap <F10> :echo "highlight<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

        " enter file explorer in directory of current buffer
""noremap - :Explore<cr>

        " enter file explorer in current directory of shell
""noremap _ :Explore .<cr>

        " write with sudo 
""cnoremap W w !sudo tee > /dev/null %

" indent guides ------------------------------------------------------------------------------------------------------------------------------------------------------
        
augroup indentguidetoggle
    autocmd!
    autocmd WinEnter,BufEnter * set conceallevel=2 | set concealcursor=nvic | syntax match IndentGuide /\v    /ms=e conceal cchar=³
augroup END

highlight Conceal ctermbg=0 ctermfg=2
        ""asdf
" Line numbering -----------------------------------------------------------------------------------------------------------------------------------------------------

        " always have absolute line numbering on
set number
""set relativenumber

        " use hybrid numbering on current window and absolute numbering on idle windows
augroup numbertoggle
    autocmd!
    autocmd WinEnter,FocusGained,InsertLeave * set relativenumber
    autocmd WinLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

        " make line number column background darkgrey and foreground black
highlight LineNr ctermbg=8 ctermfg=0
