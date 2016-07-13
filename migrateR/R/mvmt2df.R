
  ##----------------------------------------------------------------------------
  mvmt2df <- function(tmvmt,...){
    modt <- !all(sapply(tmvmt, function(x){ 
      all(class(x)%in%c("nls","glm","lm")|is.na(x))
    }))
    if (modt){
      stop("input must be a \"list\" of \"nls\", \"glm\" and/or \"lm\" objects 
        as output by \"topmvmt\"")
    }
    
    classes <- names(tmvmt)    
    names(tmvmt) <- attr(tmvmt,"burst")
    if (is.null(names(tmvmt))){
      warning("\ttmvmt lacks the \'burst\' attribute", 
        " (required for identifying bursts)\n\t\t\t\t",
        "bursts will be numbered instead",
        immediate.=T,call.=F)
      names(tmvmt)<- 1:length(tmvmt)
    }
    if (length(mc <- which(classes%in%""))>0){
      warning("\tno models included for burst(s):\n\t\t\t\t",
        paste(names(tmvmt)[mc],collapse=", "),immediate.=T,call.=F)
      tmvmt <- tmvmt[-mc]
      classes <- classes[-mc]
    }
    
    tbl <- sapply(sort(unique(classes)),function(x) {
      tbl1 <- t(sapply(tmvmt[classes%in%x], coef))
      if(any(grepl("[.]",colnames(tbl1)))){
         v <- strsplit(colnames(tbl1)[1],"[.]")[[1]][2]
         tbl1 <- t(tbl1)
         rownames(tbl1) <- names(tmvmt)[classes%in%x]
         colnames(tbl1) <- gsub(v,"Intercept","gamma")
     }
	tbl1 <- as.data.frame(tbl1)
	},simplify=F)
    return(tbl)
  }
