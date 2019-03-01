function! IndentLevel(lnum)
    return indent(a:lnum) / &shiftwidth
endfunction

function! NextNonBlankLine(lnum)
    while current <= numlines
        if getline(current) =~? '\v\S'
            return current
        endif

        let current += 1
    endwhile

    return -2
endfunction

function! IndentBlockEnd(lnum)
    while current <= numlines
        if getline(current) =~? '\v\S'
            let current += 1
        endif

        if IndentLevel(current) == IndentLevel(a:lnum) - 1
            return current
        endif
        let current += 1

    endwhile
    return -2
endfunction

""function! FoldFunc(lnum)
""    if IndentLevel(v:lnum + 1) 
""
""endfunction
