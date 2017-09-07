
 #-----------------------------------------------------------------------------  
  .mvmt.elev <- function(output){
      paramO  <- get("p.est", envir = parent.frame(1))
      
  tryCatchNull <- function(expr){
    W <- NULL
    w.handler <- function(w){ # warning handler
	  W <<- w
	  invokeRestart("muffleWarning")
    }
    cinf=list(stopCode=111)
    withCallingHandlers(tryCatch(expr, error = function(e) e),
				     warning = w.handler)	
  }
    lapply(output,function(z){
      param <- z@param
      elev <- z@data$elev[!z@data$cut]
      dday <- z@data$decday[!z@data$cut]
      control1 <- nls.control(maxiter=150,warnOnly=T) 
      ow <- options("warn")
      options(warn=-1)

      z@models$resident <- glm(elev~1)

      dis <- list()	
      dis[[1]] <- tryCatchNull(nls(
        elev~(gamma+delta/(1+exp((theta-dday)/phi))), 
	    algorithm = "port", upper = param["upr",c(1:4)], 
	    lower = param["lwr",c(1:4)], start = param["strt",c(1:4)], 
	    control = control1)
	    )
      if (class(dis[[1]])[1]!="nls"){
      	dis[[1]]<- list(convInfo=list(stopCode=111),
      	  message=as.character(dis[[1]]))
      	dis[[1]]$message <- gsub("\n","",strsplit(dis[[1]]$message,": ")[[1]][2])
      }
      if (dis[[1]]$convInfo$stopCode > 6 & all(is.na(paramO$delta[c(1,3)]))){
      	param$delta <- -rev(param$delta)
      	z@param <- param #
      	dis[[2]] <- tryCatch(nls(
          elev~(gamma+delta/(1+exp((theta-dday)/phi))), 
	      algorithm = "port", upper = param["upr",c(1:4)], 
	      lower = param["lwr",c(1:4)], start = param["strt",c(1:4)], 
	      control = control1),
	      error=function(c){
	        msg <- conditionMessage(c)
	        #message(c)
		return(c)
	    })
      }      
      if(any(sapply(dis,class)=="nls")){
      	dis <- dis[which(sapply(dis,class)=="nls")]
      	if (length(dis)>1){
          if (dis[[2]]$convInfo$stopCode==300) dis <- dis[1]
          dis <- dis[which.min(sapply(dis,AIC))] 		
        }
	z@models$disperser <- dis[[1]]	
      } else{
      	z@models$disperser <- NULL
      }

      mig <- list()
      mig[[1]] <- tryCatchNull(nls(
        elev~(gamma+delta/(1+exp((theta-dday)/phi)) - 
        delta/(1+exp((theta+phi*2+phi2*2+rho-dday)/phi2))), 
        algorithm = "port", upper = param["upr",], lower = param["lwr",], 
        start = param["strt",], control = control1))
      if (class(mig[[1]])[1]!="nls"){
      	mig[[1]]<- list(convInfo=list(stopCode=111),
      	  message=as.character(mig[[1]]))
      	mig[[1]]$message <- gsub("\n","",strsplit(mig[[1]]$message,": ")[[1]][2])
      }
      if((mig[[1]]$convInfo$stopCode > 6&class(dis[[1]])[1]=="nls")){
        param["strt",c("gamma","theta","phi","delta")] <- coef(dis[[1]])
        if (param$delta[2]>param$delta[3]){
          param$delta[c(1,3)] <- -param$delta[c(3,1)]
        }
        mig[[2]] <- tryCatch(nls(
          elev~(gamma+delta/(1+exp((theta-dday)/phi)) - 
          delta/(1+exp((theta+phi*2+phi2*2+rho-dday)/phi2))), 
          algorithm = "port", upper = param["upr",], lower = param["lwr",], 
          start = param["strt",], control = control1),
          error=function(c){
	        msg <- conditionMessage(c)
	    })
        mig <- mig[which(sapply(mig,class)=="nls")]
        if (length(mig)>1) mig <- mig[which.min(sapply(mig,AIC))] 		
      }
      
      if(any(sapply(mig, class)=="nls")){
      	mig <- mig[which(sapply(mig, class)=="nls")]
      	if (length(mig)>1){
          if (mig[[2]]$convInfo$stopCode==300) mig <- mig[1]
          mig <- mig[which.min(sapply(mig, AIC))] 		
      	}
        z@models$migrant <- mig[[1]]
      } else{
        z@models$migrant <- NULL
      }
	    
      z@models$mixmig <- z@models$nomad <- NULL
      options(ow)
      return(z)
      })	
  }
  
