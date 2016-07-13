
  delta2 <- function(mvmt){
    if (class(mvmt)[1]=="mvmt"){
      mvmt <- list(mvmt)
    }
    test1 <- do.call("rbind",lapply(mvmt,function(v){
      if (is.null(v@models$mixmig)){ 
        warning(paste("\tNo 'mixmig' model for '",attr(v,"burst"),"'",sep=""),
        call.=F,immediate.=T)
        out <- data.frame(delta2=NA,SE=NA)
        rownames(out)<- attr(v,"burst")
        return(out)
      }     
      x <- car::deltaMethod(v@models$mixmig,"delta*zeta")
      names(x) <- c("delta2", "SE")
      rownames(x) <- attr(v,"burst")
      return(x)
    }))
  }
