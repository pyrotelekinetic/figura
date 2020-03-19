setlocal softtabstop=2
syntax match Lambda contained "\\" conceal cchar=λ
syntax match Case contained "case"
syntax match LambdaCase "\\case" contains=Lambda,Case
setlocal conceallevel=2
setlocal concealcursor=nvic
highlight Case ctermfg=magenta
highlight Lambda ctermfg=magenta
highlight Conceal ctermfg=magenta ctermbg=none
