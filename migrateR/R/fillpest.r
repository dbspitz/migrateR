

  #  Fill Missing Paramter Estimates (follows fillmvmt)
  #-----------------------------------------------------------------------------
  .fillpest <- function(mvmt){
    invisible(lapply(mvmt,function(z){

	  fam <- attr(z,"fam")
	  x <- z@data$decday
	  y <- z@data[,fam]
	  censored <- which(z@data$cut)
      if(length(censored) > 0){
        x <- x[-censored]
        y <- y[-censored]
      }
      rjd <- range(x)
      drjd <- diff(rjd)
      
      
      if(grepl("nsd",fam)){
        param <- attr(z,"param")
        d2na <- is.na(param$zeta)
        if(any(d2na)) param$zeta[d2na] <- c(0, .5, 1)[d2na]		
      } else {
        param <- attr(z,"param")[,-c(7:8)] 
      }     

      gna <- is.na(param$gamma)
      if(any(gna)) param$gamma[gna] <- c(min(y),mean(y),max(y))[gna]

      tna <- is.na(param$theta)
      if(any(tna)) param$theta[tna] <- c(rjd[1], mean(rjd),rjd[2])[tna]

      rna <-is.na(param$rho)
      if(any(rna)) param$rho[rna] <- c(1, rjd[2]/3, drjd)[rna]

      dna <- is.na(param$delta)
      if(any(dna)){
      	if(grepl("nsd",fam)){
      	  param$delta[dna] <- c(0,diff(range(y))/2,diff(range(y)))[dna]
      	}else{      	
          x <- x[z@data$decday < (365 * 0.5)]
          y <- y[z@data$decday < (365 * 0.5)]
          B2 <- coef(lm(y~x))[2]
          dre <- (-1)^(B2 < 0)*diff(range(y))
          if(any(dre<param$delta[1],na.rm=T)){
            if(-dre>param$delta[1]){
              dre <- -dre              
            }else{
            stop("lower delta estimate higher than available range")
            }
          }#
          newd <- c()
          newd[2*(B2 < 0) + 1] <- 0
          newd[2*(B2 > 0) + 1] <- dre
          if(any(newd[1:2]>param$delta[3],na.rm=T)|any(newd[2:3]<param$delta[1],na.rm=T)){
          	newd <- -rev(newd)
          }
          param$delta[dna] <- newd[dna]
          if(is.na(param$delta[2])) param$delta[2] <- mean(param$delta[c(1,3)])
        }
      }
      if(length(above <- which(param["strt",] > param["upr",])) > 0){
      	param["strt",above] <- param["lwr", above] + diff(param[c(1,3), above])/2
      }
      if(length(below <- which(param["strt",] < param["lwr",])) > 0){
      	param["strt", below] <- param["lwr", below] + diff(param[c(1,3), below])/2
      }
      z@param <- param
      return(z)
    }))
  }

