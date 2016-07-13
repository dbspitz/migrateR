
  #  Even Spacing (adding spaces to front)
  ##----------------------------------------------------------------------------

  even.space <- function(x,front=T,lead=F){
    nmax <- max(nchar(x))
    spaced <- sapply(1:length(x),function(y){
      nspace <- nmax - nchar(x[y]) + lead*(y < 10)
      if(nspace > 0){
        space <- paste(rep(" ",nspace),collapse="")
      } else {
        space <- ""
      }
      if(front){
        return(paste0(space, x[y], collapse=""))
      }else{
        return(paste0(x[y], space, collapse=""))
      }
    })
    return(spaced)
  }
  