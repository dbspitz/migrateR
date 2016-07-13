
		# green	 = mixed migrant
		# blue	 = migrant
		# purple = disperser
		# orange = nomad
		# red 	 = resident
  ##----------------------------------------------------------------------------

  #require(migrateR)
  pcol <- c("darkgreen","blue","purple","orange","red")
  plty <- c(2,1,3:5)
  ylabs <- c(expression("NSD "(Km^2)),"Elevation (m)")
  x1 <- matrix(1:366,ncol=1)
  parame <- pEst()
    parame$gamma <- c(0,300,400)
    parame$delta 	<- c(0,150,500)
    param <- parame[,-1]
	parame <- parame[,-c(7,8)]
	parame$delta <-	c(-400,-300,0)

  
		
  dday <- c(1:366)
  nsd <- matrix(nrow = 366, ncol = 5)
    nsd[,1] <- c(rep(1,100),seq(2,300,length.out=18),rep(300,130),seq(299,100,length.out=18),rep(100,100))+rnorm(366)
    nsd[,2] <-c(rep(1,110),seq(1,280,length.out=18),rep(280,110),seq(280,1,length.out=18),rep(1,110))
    nsd[,3] <- c(rep(1,120),seq(1,220,length.out=26),rep(220,220))+rnorm(366)	#rep(c(1,260),each=183)
    nsd[,4] <- seq(0,180,length.out=366)
    nsd[,5] <- c(1:16,rep(17,350))+rnorm(366)
  
  control1=nls.control(maxiter=150,warnOnly=T)  # sets nls controls | #tol=1e-10
  nsdm <- list()
      mm.p <- c("theta","phi","delta","rho","phi2","zeta")
      nsdm[[1]] <- nls(nsd[,1]~(
        delta/(1+exp((theta-dday)/phi)))+
        (-(delta*zeta)/(1+exp((theta+2*phi+2*phi2+rho-dday)/phi2))), 
        algorithm = "port", start = param["strt", mm.p],
        lower = param["lwr", mm.p], upper = param["upr", mm.p],
        control = control1
      )

      mig.p <- c("theta","phi","delta","rho","phi2") 
      nsdm[[2]] <- nls(nsd[,2]~(
        delta/(1+exp((theta-dday)/phi)))+
        (-delta/(1+exp((theta+2*phi+2*phi2+rho-dday)/phi2))),
        algorithm = "port", start = param["strt", mig.p],
        lower = param["lwr", mig.p], upper = param["upr", mig.p],
        control = control1
      )

      dis.p <- c("theta","phi","delta")
      nsdm[[3]] <- nls(nsd[,3]~(delta/(1+exp((theta-dday)/phi))),
        algorithm = "port", start = param["strt", dis.p],
        lower = param["lwr", dis.p], upper = param["upr", dis.p],
        control = control1
      )

      nsdm[[4]] = glm(nsd[,4]~-1+dday)		

      p.res <- c("delta","kappa")
      nsdm[[5]] <- nls(nsd[,5]~(delta*(1-exp(kappa*dday))),
        algorithm = "port",
        start = param["strt",p.res],
		lower = param["lwr",p.res], 
		upper = param["upr",p.res] ,
		control = control1
      )

      nsdf <- sapply(nsdm,fitted)
        nsdf[,1] <- nsdf[,1]+10
        nsdf[,2] <- nsdf[,2] +5

	
  # ELEV:
  elv <- matrix(nrow=366,ncol=3)
    elv[,1] <-c(rep(300,110),seq(300,10,length.out=18),rep(10,110),seq(1,300,length.out=18),rep(300,110))
    elv[,2] <- c(rep(290,100),seq(280,1,length.out=26),rep(1,240))+rnorm(366)
    elv[,3] <- rep(310,366)+rnorm(366)

  param <- parame
  
  elvm <- list()
    elvm[[1]] <- nls(elv[,1]~(gamma+delta/(1+exp((theta-dday)/phi)) - delta/(1+exp((theta+phi*2+phi2*2+rho-dday)/phi2))), 
      algorithm="port", upper=param[3,], lower=param[1,], start=param[2,], control=control1)
    elvm[[2]] <- nls(elv[,2]~(	gamma+delta/(1+exp((theta-dday)/phi))), 
			algorithm="port", upper=param[3,c(1:4)], lower=param[1,c(1:4)], start=param[2,c(1:4)],
			control=control1)
    elvm[[3]] <- glm(elv[,3]~1)


  ##----------------------------------------------------------------------------  
  par(mfrow=c(1,2), mar=c(1.3,1.3,1.3,1.3))

  plot(cbind(c(0,366),c(0,340)),typ="n",xlab="", ylab="", xaxt="n",yaxt="n", las=1)
    title(xlab="Days",ylab=ylabs[1],line=0)
    matplot(x1,nsdf,add=T,typ="l",lty=plty,lwd=2,col=pcol,xlim=c(0,365))	
  legend("topleft",legend=c("mixed migrant", "migrant","disperser","nomad","resident"), col=pcol,lty=plty,lwd=2,cex=.58)

  plot(cbind(c(0,366),c(0,340)),typ="n",xlab="", ylab="", xaxt="n",yaxt="n", las=1)
    title(xlab="Days",ylab=ylabs[2],line=0)
    matplot(x1,sapply(elvm,fitted),add=T,typ="l",lty=plty[c(2,3,5)],lwd=2,col=pcol[c(2,3,5)],xlim=c(0,365))	
  legend("bottomleft",legend=c("migrant","disperser","resident"), col=pcol[c(2,3,5)],lty=plty[c(2,3,5)],lwd=2,cex=.58)

