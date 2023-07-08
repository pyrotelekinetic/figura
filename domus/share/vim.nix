{ pkgs, lib, ... }: {

programs.vim = {
  enable = true;
  defaultEditor = true;
  plugins = lib.mkForce [ ]; # Don't install vim-sensible
  extraConfig = ''
    " *** Misc. ***

    " hide buffers when they are abandoned
    set hidden

    " better command line completion
    set wildmode=list:longest,longest:full

    " turn on the 'visual bell' - which is much quieter than the 'audio blink'
    set visualbell

    set undofile
    set undodir=~/.vim/undo

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

    " show spaces as · when list mode is on
    set listchars=space:·,multispace:···+,tab:▸\ 
    set list

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
    \ 'ctermfgs': ['red', 'lightmagenta', 'lightyellow', 'lightgreen', 'lightblue', 'cyan', 'lightred', 'magenta', 'yellow', 'green', 'blue', 'lightcyan']
    \ }

    " highlight trailing whitespace
    highlight TrailingSpace ctermbg=red
    autocmd InsertEnter * match TrailingSpace /\s\+\%#\@<!$/
    autocmd InsertLeave,WinEnter * match TrailingSpace /\s\+$/

    " *** Indent settings ***

    " move through groups of two spaces like a single tab
    set shiftwidth=2

    " one tab is two spaces
    "set softtabstop=0
    set tabstop=2

    " automatically convert/expand tabs to spaces
    "set expandtab
    set noexpandtab

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
    cnoremap <F1> w !sudo tee > /dev/null %

    " trim trailing whitespace from current line
    noremap <F4> :s/\s\+$//e<CR>

    " toggle showing spaces as dots
    noremap <F2> :set list!<CR>

    " switch buffers on ctrl j and ctrl k
    noremap <C-j> :bn<CR>
    noremap <C-k> :bp<CR>

    " *** Line numbering ***

    " turn on relative line numbering on startup
    set number
    set relativenumber

    " use relative numbering on current window and absolute numbering on idle windows
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

    autocmd FileType gitcommit setlocal spell

    " From <https://vim.fandom.com/wiki/Avoid_scrolling_when_switch_buffers>
    " Save current view settings on a per-window, per-buffer basis.
    function! AutoSaveWinView()
      if !exists("w:SavedBufView")
        let w:SavedBufView = {}
      endif
      let w:SavedBufView[bufnr("%")] = winsaveview()
    endfunction

    " Restore current view settings.
    function! AutoRestoreWinView()
      let buf = bufnr("%")
      if exists("w:SavedBufView") && has_key(w:SavedBufView, buf)
        let v = winsaveview()
        let atStartOfFile = v.lnum == 1 && v.col == 0
        if atStartOfFile && !&diff
          call winrestview(w:SavedBufView[buf])
        endif
        unlet w:SavedBufView[buf]
      endif
    endfunction

    " When switching buffers, preserve window view.
    if v:version >= 700
      autocmd BufLeave * call AutoSaveWinView()
      autocmd BufEnter * call AutoRestoreWinView()
    endif
  '';
};

home.file.vim-dir = {
  source = ./.vim;
  target = ".vim";
  recursive = true;
};

}
