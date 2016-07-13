
  ##----------------------------------------------------------------------------
  findrloc <- function(
    ltraj, max.rloc = 15, p.est = pEst(), stdt = NA, graph = F, scut = NA, ecut = NA, verbose = F){
					
    # Check Arguments
    if (!inherits(ltraj, "ltraj")) stop("ltraj should be of class \"ltraj\"")			
    datetest <- function(x) is.na(strptime(paste(1900,x,sep="-"),"%Y-%m-%d"))
    timewarn <- "should be in \"%m-%d\" format \n\t  (see \"?strptime\" for more information)"
    if (!is.na(stdt)) {if (datetest(stdt)) {stop(paste("stdt",timewarn))}}
    if (!is.na(scut)) {if (datetest(scut)) {stop(paste("scut",timewarn))}}
    if (!is.na(ecut)) {if (datetest(ecut)) {stop(paste("ecut",timewarn))}}
    if (max.rloc<1) {stop("max.rloc must be > 1 (for rloc=1 rNSD=NSD)")}
    if (datetest(max.rloc)&!is.numeric(max.rloc)){
      stop(paste("max.rloc",gsub("be","be numeric or",timewarn)))
    }
    
    cat("Finding best supported rNSD for",length(ltraj),
      "trajectories. This may take a moment.\n")	
    
    bursts <- even.space(burst(ltraj), lead = T)
    d.f <- data.frame(burst = bursts, location = Sys.time(), rloc = max.rloc, 
      model = "disperser", row.names=1:length(ltraj), stringsAsFactors = F)

    i <- 0

    results <- lapply(1:length(ltraj),function(y){
      if (verbose) cat("\n")
      i <<- i + 1
      z <- ltraj[y]
      
      if (is.numeric(max.rloc)){
      	rlocs<- 1:max.rloc
      } else {
        if (!is.na(stdt)) {
        	d1 <- z[[1]]$date[1]            
        } else {
            d1 <- paste(as.POSIXlt(z[[1]]$date[1])$year+1900, stdt,sep="-")
        }
        d1 <- strptime(d1,"%Y-%m-%d")
        d2 <- strptime(paste(d1$year+1900,max.rloc,sep="-"),"%Y-%m-%d")
        befored2 <- difftime(z[[1]]$date,d2,un="d")<=0
        afterd1 <- difftime(z[[1]]$date,d1,un="d")>=0
        rlocs <- which(befored2&afterd1)        
      }

      if (length(rlocs)<1){
      	warning("no valid reference dates in specified range; returning NSD")
      	rlocs <- 1
      }

      res <- sapply(rlocs,function(x){
        suppressWarnings(
          mvmtClass(z,fam="nsd",p.est=p.est,stdt=stdt,rloc=x,scut=scut,ecut=ecut)
        )
      })
     class(res) <- c("mvmts","list")
     tmvmt <- topmvmt(res)
      mAIC <- sapply(tmvmt,function(x){AIC(x)})

if (verbose) print(data.frame(rloc=rlocs,topmvmt=names(tmvmt),AIC=round(mAIC)))

      if(graph){
        plot(z[[1]]$date[rlocs], mAIC, typ = "l", xlab = "Date", ylab = "AIC",
          main = attr(z[[1]],"burst"))
      }
      
      ri <- which.min(mAIC)
      d.f$rloc[i] <- ri
      d.f$location[i] <- z[[1]]$date[ri]
      d.f$model[i] <- names(mAIC)[ri]      
      out <- d.f
      out$burst <- burst(ltraj)[i]
      d.f$model <- even.space(c("disperser",d.f$model))[-1]
      d.f$rloc <- even.space(c(max(rlocs),d.f$rloc))[-1]
      if(i==1|verbose){
        print(d.f[i,])
      } else {
        if (is.numeric(max.rloc)) {d.f$rloc[i] <- paste(" ", d.f$rloc[i])}
        write.table(d.f[i,], col.names = F, row.names = T, quote = F)
      }
      return(out[i,])
	})
    results <- do.call("rbind",results)
    invisible(results)
  }
  