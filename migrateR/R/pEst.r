  
  #-----------------------------------------------------------------------------  
  pEst <- function(
    l.g = NA, l.t = 1,   l.p = 1,  l.d = NA, l.r = 1,   l.p2 = 1,  l.z = 0.05,  l.k = -1,
    s.g = NA, s.t = 90,  s.p = 7,  s.d = NA, s.r = 90,  s.p2 = 7,  s.z = 0.5,  s.k = -0.5,
    u.g = NA, u.t = 364, u.p = 21, u.d = NA, u.r = 364, u.p2 = 21, u.z = 0.95,    u.k = -0.01
    ){
	  default=c(
	    l.g, s.g, u.g,
	    l.t, s.t, u.t,
	    l.p, s.p, u.p,
	    l.d, s.d, u.d,
	    l.r, s.r, u.r,
	    l.p2, s.p2, u.p2,
	    l.z, s.z, u.z,
	    l.k, s.k, u.k
	  )			
		res <- matrix(default,3,8)
		res <- as.data.frame(res, row.names = c("lwr", "strt", "upr"))
		names(res) <- c("gamma", "theta", "phi", "delta", "rho", "phi2", "zeta", "kappa")
		if(any(res["lwr",]>res["strt",]|res["lwr",]>res["upr",],na.rm=T)){
		  stop("lower bound can not exceed starting value or upper bound")
		}
		if(any(res["upr",]<res["strt",]|res["upr",]<res["lwr",],na.rm=T)){
		  stop("upper bound must exceed starting value and lower bound")
		}
		res
	}
