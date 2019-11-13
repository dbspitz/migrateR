

  #  Fill Mvmt Object (Pre-Model Fit)
  #-----------------------------------------------------------------------------
  .fillmvmt <- function(ltraj, fam, p.est, stdt, rloc, scut, ecut){
  	mapply(function(z,rloc,scut,ecut){     
      output <- new("mvmt")
      attr(output,"burst") <- attr(z,"burst")
      attr(output,"family") <- fam
      d1lt <- as.POSIXlt(z$date[1])
      syr <- d1lt$year + 1900     
      dates <- c(stdt = stdt, scut = scut, ecut = ecut, rloc = rloc, styr = syr, dday = 0)
    
      yr <- 365*24*60*60
      if(is.na(scut)){
        scut <- z$date[1] 
      } else {        
        scut <- strptime(paste(syr,scut,sep="-"),"%Y-%m-%d")        
        if(all(difftime(scut,z$date) < 0) & any(difftime(scut+yr,z$date)>0)){
          scut <- scut + yr
        }
      }      

      if(is.na(ecut)){
        ecut <- z$date[nrow(z)]
      } else {
        ecut <- strptime(paste(syr,ecut,sep="-"),"%Y-%m-%d")
        if(difftime(scut,ecut) > 0){
          ecut <- ecut + yr
        }
      }
      if(is.na(stdt)) {
        d1 <- z$date[1] 
        dates[1] <- paste(d1lt$mon+1,d1lt$mday,sep="-")
        dates[6] <- difftime(strptime(paste(syr,dates[1],sep="-"),"%Y-%m-%d"), d1, units="days")
      } else {
	      d1 <- strptime(paste(syr,stdt,sep="-"),"%Y-%m-%d")
        if(sum(z$date<d1)*2 > length(z$date)){
          dates["syr"] <- syr <- syr-1
          d1 <- strptime(paste(syr,stdt,sep="-"),"%Y-%m-%d")
        }
	    }	 

	    dday <- as.numeric(difftime(z$date,d1,units="days"))
	    attr(output,"dates") <- dates
      infoloc <- attr(z,"infolocs")
      if(fam%in%names(infoloc)) { y <- infoloc[,fam] } else {
        if(is.na(rloc)) rloc <- 1
	      y <- switch(sum(grepl("nsd",fam),grepl("3d",fam),grepl("std",fam)),
		    ((z$x-z$x[rloc])**2+(z$y-z$y[rloc])**2)/1e6,			#	2 nsd (km)
		    (infoloc$elev-infoloc$elev[rloc])**2/1e6		+
			  ((z$x-z$x[rloc])**2+(z$y-z$y[rloc])**2)/1e6,		#	3	nsd3D (km)
		    std(infoloc$elev-infoloc$elev[rloc])**2		+
			  std(z$x-z$x[rloc])**2+std(z$y-z$y[rloc])**2)		#	4 nsd3Dstd
      }
      if(!"cut"%in%names(infoloc)) infoloc$cut <- F
      cutp <- z$date < scut | z$date > ecut | infoloc$cut
      output@data <- data.frame(dday = dday, y = y, cut = cutp)      
      if(any(is.na(y))){
        n <- sum(is.na(y))
        output@data <- output@data[which(!is.na(y)), ]
        warning("\t", n, " locations deleted due to missing ", fam, " data", immediate. = T)
      }
      names(output@data) <- c("decday",fam,"cut")
      output@param <- p.est
      return(output)
    }, z = ltraj, rloc = rloc, scut = scut, ecut = ecut)
  }

