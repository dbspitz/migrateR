
  #-----------------------------------------------------------------------------
  mvmtClass <- function(ltraj, fam = "nsd", p.est = pEst(), stdt = NA,
    scut = NA, ecut = NA, rloc = NA, warnOnly = F){	
				
    if (!inherits(ltraj, "ltraj")) stop("ltraj should be of class \"ltraj\"")
	if (length(fam)>1){
	  warning(
	    "\"fam\" has length > 1 and only the first element will be used",
	  call. = F)
	  fam <- fam[1]
	}
	if(length(stdt)>1){
	  warning(
	    "\"stdt\" has length > 1 and only the first element will be used",
	  call. = F)
	  stdt <- stdt[1]
	}
	
    datetest <- function(x){
    	x <- x[which(!is.na(x))]
    	length(x>0) & is.na(strptime(paste(1900,x,sep="-"),"%Y-%m-%d"))
    }
    twarn <- "must be in \"%m-%d\" format (see \"?strptime\" for details)"
	format.test <- function(x){
      z <- get(x)
	  if(any(datetest(z))) stop (paste(x, twarn), call. = F)	   
	}
	
	format.test("stdt")
	format.test("scut")
	format.test("ecut")

    mvmt.fam <- c("nsd","nsd3d","nsd3d.std","elev")
    new.fam <- ls(.GlobalEnv, pattern = "mvmt[.]")
    if (length(new.fam)>0) mvmt.fam <- unique(c(mvmt.fam,new.fam))
    
    if (!fam%in%mvmt.fam){
      stop(paste("\"fam\" does not match available model families", 
       "\n\t\t(possible values include: ",
        paste(mvmt.fam,collapse=", "),")",sep="")
      )
    }

	if (!is.na(rloc[1])){
      if (grepl("nsd",fam)){
	    if (length(rloc)!=length(ltraj)) 
        stop("length of \"rloc\" does not match length of \"ltraj\"")  
	  }
      if (!grepl("nsd",fam))
       warning("argument 'rloc' not meaningful for fam = ",fam)
    }
	if(fam!="nsd"){
	  if(is.null(infolocs(ltraj)))stop("ltraj missing required infolocs field")
    }      
   	
    output <- .fillmvmt(ltraj,fam, p.est, stdt, rloc, scut, ecut)
    output2 <- .fillpest(output)
    if(grepl("nsd",fam)) fam <- "nsd"
    helper <- paste(".mvmt",fam,sep=".")
	results <- eval(call(helper,output2))	
    names(results) <- burst(ltraj)
    cmsg <- c("singular convergence (7)","false convergence (8)")
    wresults <-  sapply(names(results),function(z){	
      msg <- sapply(results[[z]]@models,function(y) y$message)
      n <- which(msg%in%cmsg)
      if(length(n)>0){
        warning("convergence problem(s) for \"", z, "\"", paste("\n\t", 
          names(msg)[n], sapply(10-nchar(names(msg))[n],function(x){
            paste(rep(" ",x),collapse="")
          }),":\t\t", msg[n],"\n", sep=""), call. = F, immediate. = T
        )
        results[[z]]@models[n] <- NULL
#        results[[z]]@models <- results[[z]]@models[-n]
      }
      return(results[[z]])
    })
    if(!warnOnly){results <- wresults}

    class(results) <- c("mvmts","list")
    results
  }

