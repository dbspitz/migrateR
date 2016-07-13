matcheck <-
function(x){	# x=modlst$migrant
	test=x$m$Rmat()
	if(length(which(test==0))>sum(1:(nrow(test)-1))){
		grad=x$m$gradient()
		grad[which(grad==0)]=1e-10
	
		biM=c()
		for(i in 1:ncol(grad)){
			fill=c(rep(1,i),rep(0,ncol(grad)-i))
			biM=c(biM,fill)
		}
		
		biM=matrix(data=biM,ncol=ncol(grad),nrow=ncol(grad))
		(output=qr(grad)$qr[1:ncol(grad),]*biM)	
	}
}
