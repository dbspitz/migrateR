
  ##----------------------------------------------------------------------------
  mvmt2dt <- function(mvmt,p=0.05,mod="migrant"){
    if (!grepl("mvmt",class(mvmt)[1])) 
      stop("mvmt should be of class \"mvmts\" or \"mvmt\"")
    if (p > 0.5) stop("p can not be > 0.5")
    if (p <= 0) stop("p must be > 0")
    if (!is.list(mvmt)){
      mvmt <- list(mvmt)
      class(mvmt) <- c("mvmts","list")
    }
   
    sapply(mvmt,function(x){
      if (!mod%in%names(x@models)){ 
        warning(paste("\tNo '",mod,"' model for ",attr(x,"burst"),sep=""),call.=F)
        return(NULL)
      }
      if (mod%in%c("nomad","resident")){
        stop(paste(mod, "models do not characterize migration or dispersal events"))
      }
      btbl <- coef(x@models[[mod]])
      theta <- btbl["theta"]
      phi <- btbl["phi"]    
      jdts <- theta - phi*log((1-p)/p)
      jdts[2] <-theta - phi*log((p)/(1-p))
      if (grepl("mig",mod)){
        theta.2 <- theta2(x,mod=mod)$theta2[[1]]
        phi2 <- btbl["phi2"]
        jdts[3] <- theta.2 - phi2*log((1-p)/p)
        jdts[4] <- theta.2 - phi2*log((p)/(1-p))
        if (jdts[2]>jdts[3]){
          warning(paste("\t",attr(x,"burst")," movements overlap",sep=""),call.=F)
        }
      }
      dts <- attributes(x)$dates
      stdt <- as.POSIXct(paste(dts["styr"],dts["stdt"],sep="-"))
      cdt <- stdt+60*60*24*jdts
      dates <- data.frame(dday=jdts,date=cdt)
      rownames(dates) <- c("str1","end1","str2","end2")[1:nrow(dates)]    
      return(dates)
    },simplify=F)
  }