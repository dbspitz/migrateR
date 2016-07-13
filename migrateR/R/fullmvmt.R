
  ##----------------------------------------------------------------------------
  fullmvmt <- function(mvmt, out = "logic"){
    if (!grepl("mvmt",class(mvmt)[1])) 
      stop("mvmt should be of class \"mvmts\" or \"mvmt\"")
    if (!is.list(mvmt)){
  	  mvmt <- list(mvmt)
  	  class(mvmt) <- c("mvmts","list")
    } 
    typ <- attr(mvmt[[1]],"fam")
    y <- lapply(mvmt,function(z) names(z@models))
    ly <- sapply(y,length)
    if(out=="logic")return(ly == ((typ=="nsd")*2 + 3))
    if(out=="numer")return(ly)	
    if(out=="name") return(y)    
  }
