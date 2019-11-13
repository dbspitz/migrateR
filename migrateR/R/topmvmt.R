  
  #  mnr = minimum difference between nomad and resident ending values
  ##----------------------------------------------------------------------------
  topmvmt <- function(mvmt, omit = NA, a.rule = T, mdelta = NA, mrho = NA, mdifdelta = NA, mnr = NA){
    if (!grepl("mvmt",class(mvmt)[1])) 
      stop("mvmt should be of class \"mvmts\" or \"mvmt\"")
    if (!is.list(mvmt)){
  	    mvmt <- list(mvmt)
  	    class(mvmt) <- "mvmts" 
    } 
    modn <- names(new("mvmt")@models)
    if (attr(mvmt[[1]],"fam") =="elev") modn <- modn[c(1,2,5)]
    if (!is.na(omit[1])&!all(omit%in%modn)){
      stop("'omit' includes invlaid model name(s)--may only omit: ",
      paste(modn,collapse=", "))
    }
    names(mvmt)<-NULL
    output <- sapply(mvmt,function(x){
      aicres <- sapply(x@models,AIC)
      aicres <- sapply(modn,function(y) as.numeric(aicres[y]))
      if (!is.na(omit[1])) {                    
        omit <- match(omit, modn)
        aicres[omit] <- NA
      }
      nonres <- x@models[-length(x@models)]
      if (!is.na(mdelta)){
        a <- which(lapply(nonres, function(z) abs(coef(z)["delta"]))<mdelta)
          if (length(a)>0) aicres[a] <- NA
      }
      if (!is.na(mrho)){
          a <- which(lapply(nonres, function(z) coef(z)["rho"])<mrho)
        if (length(a)>0) aicres[a] <- NA
      }
      if (!is.na(aicres["mixmig"])&!is.na(mdifdelta)){
        difdelta <- abs(delta2(x)$delta2 - coef(nonres$mixmig)["delta"])
        if (difdelta < mdifdelta) aicres["mixmig"] <- NA
      }
      if(!is.na(aicres["nomad"])&!is.na(aicres["resident"])&!is.na(mnr)){
        dnr <- abs(diff(sapply(x@models[c("resident", "nomad")], function(z) max(fitted(z)))))
        if(dnr < mnr) aicres["nomad"] <- NA
      }
      tmod <- which.min(aicres)
      if(length(tmod)==0){
        b <- attr(x,"burst")
        warning("\tno models in burst ",b,
          " match the specified constraints",call.=F)
        return(NA)
      }
      cntdr <- which((aicres-min(aicres,na.rm=T)<=a.rule*2))
      tmod <- which.min(sapply(x@models[cntdr],function(x) length(coef(x))))
      outp <- x@models[names(cntdr)[tmod]]
      return(outp)
    })
    attr(output,"burst") <- sapply(mvmt,attr,"burst")
    return(output)
  }
  