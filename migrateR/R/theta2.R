
  ##----------------------------------------------------------------------------
  theta2 <- function(mvmt,mod="migrant"){
    if (!mod%in%c("migrant","mixmig")){
      stop(paste(mod, "models do not characterize migration events"))
    }
    if(!is.list(mvmt)){
  	  mvmt <- list(mvmt)
  	  class(mvmt) <- c("mvmts","list")
    } 
    as.data.frame(t(sapply(mvmt,function(v){
      if (!mod%in%names(v@models)){ 
        warning(paste("\tNo '",mod,"' model for '",attr(v,"burst"),"'",sep=""),
        call.=F,immediate.=T)
        out <- data.frame(theta2=NA,SE=NA)
        rownames(out)<- attr(v,"burst")
        return(out)
      }     
      x <- car::deltaMethod(v@models[[mod]],"theta+2*phi+rho+2*phi2")[1:2]
      names(x) <- c("theta2", "SE")
      return(x)
    })))
	}
