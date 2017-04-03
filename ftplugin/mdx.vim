if (exists("b:did_ftplugin"))
  finish
endif
let b:did_ftplugin = 1

setlocal comments=:--,://,s1:/*,mb:*,ex:*/ 
setlocal commentstring=--\ %s
setlocal formatoptions+=cqr
setlocal tabstop=2
setlocal shiftwidth=2
setlocal cinkeys="0{,0},0),0\,"
