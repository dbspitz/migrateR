
  #-----------------------------------------------------------------------------
  .mvmt.nsd <- function(output){
    lapply(output,function(z){
      param <- z@param
      fam <- attr(z,"fam")
      dates <- attr(z,"dates")
	  nsd <- z@data[,fam]
      dday <- z@data$decday
	  censored <- which(z@data$cut)
      if(length(censored) > 0){
        nsd <- nsd[-censored]
        dday <- dday[-censored]
      }
      dday2 <- dday - min(dday)
      
      control1 <- nls.control(maxiter=150,warnOnly=T) 
      ow <- getOption("warn")
      options(warn=-1)
      
      mm.p <- c("theta","phi","delta","rho","phi2","zeta")
      z@models$mixmig <- nls(nsd~(
        delta/(1+exp((theta-dday)/phi)))+
        (-(delta*zeta)/(1+exp((theta+2*phi+2*phi2+rho-dday)/phi2))), 
        algorithm = "port", start = param["strt", mm.p],
        lower = param["lwr", mm.p], upper = param["upr", mm.p],
        control = control1
      )
      
      mig.p <- c("theta","phi","delta","rho","phi2") 
      z@models$migrant <- nls(nsd~(
        delta/(1+exp((theta-dday)/phi)))+
        (-delta/(1+exp((theta+2*phi+2*phi2+rho-dday)/phi2))),
        algorithm = "port", start = param["strt", mig.p],
        lower = param["lwr", mig.p], upper = param["upr", mig.p],
        control = control1
      )
      test <- matcheck(z@models$migrant)
      if(!is.null(test)){
      	z@models$migrant$m$Rmat=function() {test}
      }				
      dis.p <- c("theta","phi","delta")
      z@models$disperser <- nls(nsd~(delta/(1+exp((theta-dday)/phi))),
        algorithm = "port", start = param["strt", dis.p],
        lower = param["lwr", dis.p], upper = param["upr", dis.p],
        control = control1
      )
      test <- matcheck(z@models$migrant)
      if(!is.null(test)){
      	z@models$migrant$m$Rmat=function() {test}
      }				

      p.res <- c("gamma","kappa")
      z@models$resident <- nls(nsd~(gamma*(1-exp(kappa*dday2))),
        algorithm = "port",
        start = param["strt",p.res],
		lower = param["lwr",p.res], 
		upper = param["upr",p.res] ,
		control = control1
      )

      beta <- dday2
      z@models$nomad <- glm(nsd~-1+beta,data=data.frame(beta = dday2))

      if(!is.null(test)){
      	z@models$migrant$m$Rmat=function() {test}
      }
      
      z@models <- z@models[sort(names(z@models))]
		
      options(warn=ow)
      return(z)
    })
  }

