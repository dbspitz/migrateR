
  ##----------------------------------------------------------------------------
  spatmig <- function(ltraj, mvmt, mod="migrant", graph=T, new=F){
    invisible(mapply(function(z,mvmt){
      if(!mod%in%c("migrant","mixmig","disperser")) stop("you must specify an appropriate model")
        cat(attr(z,"burst"))
        if(!mod%in%names(mvmt@models)){
          cat("\n\t")
          warning("no ", mod, " model fit for ", attr(mvmt,"burst"),
            call.=F,immediate.=T
          )          
          return(list())
        }
        answer <- readline("\tany key to continue (or \'0\' to skip)")
	    if(answer==0) return("")
	
        	
		x <- z$x
		y <- z$y
		dday <- mvmt@data$decday
		y2 <- mvmt@data[,2]
		
		dates <- attr(mvmt,"dates")
		styr <- dates["styr"]
		ddec <- as.numeric(dates[6])*24*60*60	
		stdt <- strptime(paste(styr,dates["stdt"],sep="-"),"%Y-%m-%d")-ddec 
		ecut <- dates["ecut"]
		scut <- dates["scut"]
		rloc <- as.numeric(dates["rloc"])
		
    if(is.na(ecut)){
      ecut <- z$date[nrow(z)]
    }else{
      ecut <- strptime(paste(styr,ecut,sep="-"),"%Y-%m-%d")
    }
    
    if(is.na(scut)){
      scut <- z$date[1]
    }else{
      scut <- strptime(paste(styr,scut,sep="-"),"%Y-%m-%d")	
    }
    d1 <- as.POSIXlt(z$date[1])
    ejdt <- as.numeric(difftime(ecut,stdt,units="days"))
    sjdt <- as.numeric(difftime(scut,stdt,units="days"))
    incl <- which(!mvmt@data$cut)

    if(length(incl)<1) stop("there are no points in the given range")
    
    param <- coef(mvmt@models[mod][[1]])

    if(param["delta"]>0){
      below <- y2 < mean(y2[incl],na.rm=T)
    }else{
      below <- y2 > mean(y2[incl],na.rm=T)
    }
    
    c2 <- which(dday > param[c("theta")] & 
      ((dday < sum(param[c("theta","phi","rho","phi2")]*1:2) | 
      (is.na(param["rho"])))) & !below
    )
		
    c1 <- which(
      (dday< param[c("theta")]  | 
        dday > sum(param[c("theta","phi","rho","phi2")]*1:2)
      ) & below
    )

		results <- rep(NA,length(incl))
		results[c1] <- 1
		results[c2] <- 2		

		if(graph){		
			if(new) dev.new()

			par(mfrow=1:2)
			plot(mvmt)
            leg <- data.frame(
              lab <- c("ref. loc.","range 1","range 2","unclassified"),
              col <- c(1,2,4,"grey"), 
              pch <- c(0,16,3,1),stringsAsFactors=F)

			if (length(incl)<length(x)) {
				points(dday[-incl],y2[-incl],col="darkgrey",pch=22,cex=.5)
			    leg[4,] <- c( "excluded","darkgrey")			
			}
			points(dday[c1],y2[c1],cex=.2,pch=16)
			points(dday[c2],y2[c2],cex=.4,pch=3)

			par(mar=c(5, 1, 4.1, 9.1))
			plot(x,y,xlab="",ylab="",main=mod,cex=.75,
			typ="n",las=1,yaxt="n")

			if (length(c(c1,c2))<length(incl)){
				wna <- which(is.na(results[incl]))
				points(x[wna],y[wna],
				cex=.5,col="grey")
			}
			if (length(incl)<length(x)) {
				lab[5,] <- c("censored","grey",22)
				points(x[-incl],y[-incl],col="darkgrey",pch=22,cex=.5)		
			}
			points(x[c1],y[c1],cex=.4,pch=16,col=2)
			points(x[c2],y[c2],cex=.5,pch=3,col=4)			
			
			if (is.na(rloc)){rloc <- 1}
            if (rloc==1){
			  leg$lab[1] <- "start loc."
			}

		    points(x[rloc], y[rloc], col = 1, pch = 0, cex = 1.5, lwd = 1.5)
		  
		    yaxp <- par("usr")[3:4]
			yleg <- yaxp[1]+.75*(yaxp[2]-yaxp[1])
			xaxp <- par("usr")[1:2]
			xleg <- xaxp[2]+.02*(xaxp[2]-xaxp[1])
			par(xpd=T)
			legend(xleg,yleg,title="Grouping",
				legend= leg$lab,
				pch=leg$pch,col=leg$col)
			par(xpd=F)
		      
		}
	
		invisible(results)
	},z = ltraj, mvmt = mvmt, SIMPLIFY=F))
}