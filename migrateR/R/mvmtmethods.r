

  ## I. Set Classes
  ##----------------------------------------------------------------------------
  setClass("mvmt",slots = c(data = "data.frame", param = "data.frame", 
    models = "list" ), prototype = list(data = data.frame(), 
      param = data.frame(), models = alist(
        disperser = nls(), migrant = nls(), mixmig = nls(), nomad = nls(),
        resident = nls()
      )
	)
  )

  setClass("mvmts", prototype = list("mvmt")) 

  `[.mvmts` <- function(x, i, burst){
    if (!inherits(x,"mvmts")){
      stop("x should be of class \"mvmts\"")
    }
    if(sum(!missing(i))+(!missing(burst))!=1){
      stop("unable to execute subset: refer to either numeric indexing 'i' or named 'bursts'")
    }
    if (!missing(i)){
      x <- unclass(x)
      y <- x[i]
    }
    if (!missing(burst)){
      idb <- summary(x)$burst
      x <- unclass(x)
      y <- x[idb%in%burst]
    }
    class(y)<- c("mvmts","list")
    return(y)
  }

  ## II. summary
  ##----------------------------------------------------------------------------
  summary.mvmt <- function(x, ...){
    mvmt1 <- topmvmt(x)
    decday <- x@data$decday
    dates <-attr(x,"dates")
	stdt <- strptime(paste(dates["styr"], dates["stdt"],sep = "-"),"%Y-%m-%d")    
    minday <- stdt +24*60*60*min(decday)
    maxday <- stdt +24*60*60*max(decday)
    data.frame(burst = attr(x,"burst"), topmod = names(mvmt1), 
      locs = length(decday), date.begin = minday, date.end = maxday
    )     
  }

  summary.mvmts <- function(x, ...){
    d.f <- do.call("rbind",lapply(x,summary.mvmt))
    rownames(d.f) <- 1:nrow(d.f)
    return(d.f)
  }


  ## III. str
  ##----------------------------------------------------------------------------
  str.mvmt <- function(x, ...){
    cat("Formal class 'mvmt' [package 'migrate'] with 3 slots")
    cat("\n\t@models")
    for(i in 1:length(x@models)){
      n.i <- names(x@models)[i]
      n.s.i <- max(nchar(names(x@models)))-nchar(n.i)
      s.i <- rep(" ",n.s.i,collapse = "")
      cat("\n\t\t$ ",n.i,s.i ,"\t(AIC = ",AIC(x@models[[i]]),")", sep = "")
    }
    cat("\n\t@param\t\t  [lwr, strt, upr]")
    for(i in 1:length(x@param)){
      n.i <- names(x@param)[i]
      n.s.i <- max(nchar(names(x@param)))-nchar(n.i)
      s.i <- rep(" ",n.s.i,collapse = "")
      cat("\n\t\t$ ",n.i,s.i ,"\t\t",sep = "")
      cat(round(x@param[,n.i]),sep = "\t  ")
    }

    cat("\n\t@data\t\'data.frame\':\t(",nrow(x@data)," locations)",sep = "")
      cat("\n\t\t$", names(x@data)[1], "\t", round(head(x@data[,1]), 2), "...")
      cat("\n\t\t$", names(x@data)[2], "\t\t", round(head(x@data[,2]), 2), "...")
    cat("\n\n")
  }

  str.mvmts <- function(x, ...){
    for(i in 1:length(x)){
     str(x[[i]])
    }
  }


  ## IV. print
  ##----------------------------------------------------------------------------
  print.mvmts <- function(x, ...){
  	cat("\n*********** List of class mvmts ***********\n\n")
  	
  	fams <- sapply(x,function(z){attr(z,"fam")})
  	if(all(sapply(fams,function(z){z == fams[1]}))){
  	  cat("Family:\t\t\t",fams[1],"\n")
  	}
  	stdts <- sapply(x,function(z){attr(z,"dates")[1]})
  	if(all(sapply(stdts,function(z){z == stdts[1]}))){
  	  cat("Start date:\t\t",stdts[1],"(shared origin of timing param)\n")
  	}
 	scuts <- sapply(x,function(z){attr(z,"dates")[2]})
  	if(any(!is.na(scuts))){
  	  if(all(sapply(scuts,function(z){z == scuts[1]}))){
  	    cat("Starting cutoff:\t",stdts[1],"(preceding points are exculded)\n")
  	  }
  	}
 	ecuts <- sapply(x,function(z){attr(z,"dates")[3]})
  	if(any(!is.na(ecuts))){
      if(all(sapply(ecuts,function(z){z == ecuts[1]}))){
  	    cat("Ending cutoff:\t",ecuts[1],"(following points are excluded)\n")
  	  }
  	}
   	refdts <- sapply(x,function(z){attr(z,"dates")[4]})
  	if(any(!is.na(refdts))){
      if(all(sapply(refdts,function(z){z == refdts[1]}))){
  	    cat("Reference loc:\t",refdts[1],
  	      "\t(for squared displacement calculation)\n")
  	  }
  	}
	params <- lapply(x,function(z){z@param[-2,]})
  	equalp <- sapply(names(params[[1]]),function(z){
  	  apply(
  	    sapply(params,function(a){ params[[1]][,z] == a[,z]}),1,all
  	  )
  	})
 	params[[1]][which(!equalp,arr.ind = T)]<- "-"
  	cat("\nShared parameter constraints:\n\n")
  	print(params[[1]])	
  	
    cat("\nCharacteristics of the bursts:\n")
  	print(summary(x))
  }


  ## V. plot
  ##----------------------------------------------------------------------------
  plot.mvmt <- function(mvmt, new = F, omit = NA, ranked = T, xlim = c(0,365), 
    ...){		
      fam <- attr(mvmt, "family")
      dates <- attr(mvmt, "dates")
      stdt.chr <- paste(dates["styr"],dates["stdt"],sep = "-")
      stdt <- strptime(stdt.chr,"%Y-%m-%d") + 24*60*60*as.numeric(dates["dday"])
      p <- data.frame(
       mod = c("disperser","migrant","mixmig","nomad","resident"),
       col = c("purple", "blue", "darkgreen", "orange", "red"),
        lty = c(2,1,3:5),
        stringsAsFactors = F
      )
      p <- p[which(p$mod%in%names(mvmt@models)),]

      if(new == T) dev.new()
      opar <- par(mar = c(0,0,0,0), mgp = c(3,1,0), xpd=F)
      par(mar=c(5, 4.1, 4.1, 9.1),mgp = c(2.5,0.5,0))
      
      mdata <- mvmt@data 
	  x1 <- mdata$decday[!mdata$cut]      
	  y1 <- mvmt@data[,fam]

          if (max(na.omit(c(dates["rloc"], 1)))>1)
        fam = "rnsd"

      ylab <- c(nsd = expression("NSD " (Km^2)),
        rnsd =expression("rNSD " (Km^2)),
        elev = "Elevation (m)")[fam]
      
      plot(seq(1,365,length.out = length(y1)), y1, typ = "n",
        xlab = "Days", ylab = ylab, xaxt = "n", main = attr(mvmt,"burst"), 
        las=1, xlim=xlim, ...)    
      mvmtAxis(stdt)
      
      if(sum(mdata$cut) > 0 ){
        x2 <- mdata$decday[mdata$cut]      
        y2 <- y1[mdata$cut]
        points(x2, y2, pch = 0, cex = .5, col = "darkgrey") #, xlim = xlim	
      } 
      
      y1 <- y1[!mdata$cut]            
	  points(x1, y1, cex = .5, col = "grey", xlim = xlim)
	  if(max(xlim)<max(x1))  warning(paste("some x-values > xlim; n ="
	                                     , sum(x1>max(xlim))
	                                     , "points beyond plotting range"))

      # Omit Models From Plot (?)
      omit <- match(omit,names(mvmt@models))
	  if(!is.na(omit[1])){
		mvmt@models <- mvmt@models[-omit]
		p <- p[-omit,]
	  }
	  modn <- names(mvmt@models)

      y1 <- mapply(fitted,mvmt@models)
      if(length(x1)==max(dim(y1))){  
	# Models
	matplot(x1,y1, add = T, typ = "l", 
	    lty = p$lty, lwd = 2, col = p$col, xlim = xlim)	

        # Legend
        par(xpd=T)	
	  aicres <- sapply(mvmt@models, AIC)
	  daic <- aicres - min(aicres)	
	  yaxp <- par("yaxp")
	  yleg <- yaxp[1] + 0.75 * (yaxp[2] - yaxp[1])

        # Rank Legend on AIC?
	if(ranked){
          aico   <- order(daic)
          aicres <- aicres[aico]
          p$col   <- p$col[aico]
          modn   <- modn[aico]
          daic   <- daic[aico]
          p$lty   <- p$lty[aico]
        }
	legend(max(xlim)*1.07, yleg, title = as.expression(
          substitute(A~B, list(A = as.name("Delta"),
          B = as.name("AIC")))
          ),legend = paste(round(daic, 0), modn), lty = p$lty,
          col = p$col, lwd = 2)
    }	      
      par(opar)
      invisible(attr(mvmt,"burst"))
    }  
    
  plot.mvmts <- function(mvmts, new = F, omit = NA, ranked = T,...){
    sapply(mvmts,function(x){
    cat(attr(x,"burst"))
    answer = readline("\tany key to continue (or \'0\' to skip)")
	if(answer==0) return("")
	plot(x, new=new, omit=omit, ranked=ranked, ...)   	
    }) 
  }
