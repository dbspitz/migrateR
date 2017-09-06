  refine <- function(mvmt, p.est){
      if(!is.list(mvmt)){
  	    mvmt <- list(mvmt)
  	    names(mvmt) <- attr(mvmt[[1]],"burst")
  	    class(mvmt) <- c("mvmts","list")
      } 
    
    mvmt0 <- sapply(mvmt,function(x){
      z <- attr(x,"param")
      x@param <- p.est[,names(z)]
      test <- .fillpest(list(x))
      p.est <- test[[1]]@param
      attr(x, "param") <- p.est
      return(x)
    })

    fam <- sapply(mvmt, attr, "fam")
    if(length(unique(fam))==1){
    	fam <- fam[1]
    } else {stop("more than one model family:",unique(fam))}
    if(grepl("nsd",fam)) fam <- "nsd"    
    helper <- paste(".mvmt",fam,sep=".")

	mvmt2 <- eval(call(helper,mvmt0))	
    cmsg <- c("singular convergence (7)","false convergence (8)",
      "singular gradient matrix at initial parameter estimates")    
    wmvmt2 <-  sapply(names(mvmt2),function(z){	
      msg <- sapply(mvmt2[[z]]@models,function(y) y$message)
      n <- which(msg%in%cmsg)
      if(length(n)>0){
        mvmt2[[z]]@models <- mvmt2[[z]]@models[-n]
      }
      return(mvmt2[[z]])
    })

    mvmt.f <- sapply(names(mvmt),function(x){
      mvmt1 <- mvmt[x][[1]]
      mod <- mvmt1@models
      newmod <- wmvmt2[x][[1]]@models
      og <- names(newmod)[which(names(newmod)%in%names(mod))]
      if(length(og)>0){
      	tryAIC <- function(z){
          switch(1+(class(z)[1]=="list"), AIC(z), Inf)
        }
	improved <- og[which(sapply(newmod[og], tryAIC) < 
      	  sapply(mod[og], tryAIC))]
      	if(length(improved)>0){
      	  mod[improved] <- newmod[improved]
          mvmt1@models <- mod[sort(names(mod))]
          attr(mvmt1,"param") <- attr(wmvmt2[x][[1]],"param")	
      	}
      }	  

	  wnew <- which(!names(newmod)%in%names(mod))
      if(length(wnew)>0){
        mod[names(newmod)[wnew]] <- newmod[wnew]
        mvmt1@models <- mod[sort(names(mod))]
      }        
      return(mvmt1)
    })
    class(mvmt.f) <- c("mvmts","list")
    return(mvmt.f)
  }
