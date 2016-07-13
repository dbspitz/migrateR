mvmtAxis <-
function(d1,side=1){
	jst=as.POSIXlt(d1)$yday+1
	jfsts=strptime(paste(c(1:12),"/1/11",sep=""),"%m/%d/%y")$yday+1		# fsts-> jdt
	rjfsts=mapply(function(x,y){(x-y+365)%%365},jfsts,jst)	# rotate jfsts on jst
	mth=c(1:12)
	repeat{									# DO WHILE LOOP!
		if(rjfsts[1]==min(rjfsts)){break}
		(rjfsts=rotvec(rjfsts))
		(mth=rotvec(mth))
	}
	mths=format(ISOdate(2000, mth, 1), "%b")
	axis(side,rjfsts,lab=mths,las=1)	
}
#as.POSIXlt("2011-10-31")