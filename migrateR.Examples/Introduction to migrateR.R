	##
  ##	 D. SPITZ
  ##	 2017-04-05
  ##
  ##
  ##  A BRIEF INTRODUCTION TO "migrateR"
  ##
  ##  0. Preamble: Why migrateR?
  ##	    A. Efficiency
  ##	    B. Transparency
  ##	    C. Consistency
  ##  ------
  ##  I.	The Hardest Part
  ##      A. Installing "migrateR", Jumping In
  ##      B. Formatting data
  ##      C. Defining migration
  ##
  ##  II.	Analysis 
  ##      A. Fitting Models
  ##      B. Visual Checks
  ##      C. Refining Fit
  ##
  ##  III.	Inference
  ##      A. Classification
  ##      B. Description
  ##  -----
  ##  Bonus:	Combatting Misclassification
  ##	    A. Model Families
  ##	    B. Model Selection
  ##	    C. Excluding Points
  ##
  ##
  ##  **  Note: Today I'll assume familiarity with:  **
  ##       - Program R
  ##       - Bunnefeld et al. 2011
  ##
  
  
  ##----------------------------------------------------------------------------  
  #  0.  Preamble: Why migrateR?	--------------

    # Once upon a dissertation...  
  
    #  A. Efficiency   --  less code, more use
    #  B. Transparency --  understanding => improvement
    #	 C. Consistency  --  matching methodology & ecology
  

  ##----------------------------------------------------------------------------  
  #  I.		The Hardest Part		--------------

    #  A. Installing "migrateR", Jumping In
      # To Install Package, visit:
    	# 	https://github.com/dbspitz/migrateR

        require(migrateR)		  # Load Package
        
        # Example data
        data(bighorn) 
  
        # Fit 5 non-linear models of animal movement
        bhs.nsd <- mvmtClass(bighorn[3])  # the workhorse
        
        # Take a look at the results
        plot(bhs.nsd[[1]])
    
    #  B. Formatting data

      # i.  migrateR Examples

        # bighorn sheep, Sierra Nevada Mountains, USA
        bighorn
        head(bighorn[[3]])
        plot(bighorn[3])
        # or try: trajdyn(bighorn[3])

        # elk, Canadian Rockies
        data(elk)       
        elk

      #  ii. Resources & Recomendations
        
        # Setup instructions and spatial tools (really thorough!):
        #	vignette("adehabitatLT") 
        
        # Trajectories Don't need to be regular (but watch out for gaps!)
        # Subset to 1 pt/day (If dealing with an annual cycle)
        #  (Or: take the centroid of a day's points w/e.g. rgeos::gCentroid)       
        
    
    #  C. Defining Migration
        #  Oh boy...
        #    Consider:
        #
        #    1- What hypotheses are you trying to test?
        #
        #    2- What sorts of patterns do you expect?
        #		    - What previous work has been done on migration in your system?
        #         - What assumptions are common? (& can/have these be/en tested?)
        #       - What differences do you expect from other migratory systems?
        #       - Why are animals migrating?
        #
        #    3- How well do these patterns correspond to available methods?
        #	
        # Now on to the easier stuff...
        
        
  ##----------------------------------------------------------------------------    
  #  I.	Analysis	--------------

    #      A. Fitting Models
		   
       elk.nsd1 <- mvmtClass(elk)   # yep, that's it.
       elk.nsd1

    #      B. Visual Checks

		   # Ye ol' 'plot' function
		   plot(elk.nsd1)				# Will cycle through all elements [esc!]	
		   
		   plot(elk.nsd1[[9]])  # What we want to see! -- as good as it gets
		   
		   # Plots with Missing Models: 2 Examples
       plot(elk.nsd1[["YL77 2004"]])
       plot(elk.nsd1[["YL15 2003"]])
       plot(elk.nsd1[["YL25 2003"]])
      
       # Paired model and xy plots (work in progress...)
		   spatmig(elk, elk.nsd1)		# Will cycle through all elements [esc!]	
		   
		   # Potential Problems?
		   spatmig(elk[which(burst(elk)%in%"YL58 2004")], elk.nsd1["YL58 2004"])
		   spatmig(elk[which(burst(elk)%in%"YL64 2004")], elk.nsd1["YL64 2004"])
		   spatmig(elk[which(burst(elk)%in%"YL80 2004")], elk.nsd1["YL80 2004"])
		   
       par(mfrow = c(1,1))
  
    #      C. Refining Fit
      
      # (First, let's take a closer look at the results of 'mvmtClass')
      class(elk.nsd1)
      str(elk.nsd1[[1]])
    
      #  i.  Check for Missing Models
		   
		    fullmvmt(elk.nsd1)
		    winc <- which(!fullmvmt(elk.nsd1)) # w(hich) inc(omplete)
		    winc
        fullmvmt(elk.nsd1, out = "numer")[winc]
		    fullmvmt(elk.nsd1, out = "name")[winc]
		   
      #  ii.  Change Constraints & Refit
  
        # Problem Plots can provide valuable clues
        plot(elk.nsd1[[names(winc)[6]]])
		   
        # setting a starting delta value is often a good place to start
        pEst()                     # default parameter constraints
        (pest.n <- pEst(s.d = 150))  # manually setting delta start value
		    elk.nsd1.1 <- refine(elk.nsd1, pest.n)
       
        which(!fullmvmt(elk.nsd1.1))
        plot(elk.nsd1.1[[names(winc)[6]]])
		   
      #  iii.  Rinse & Repeat
       
        pest.n2 <- pEst(s.d = 900)
  		  elk.nsd1.2 <- refine(elk.nsd1.1, pest.n2)
        which(!fullmvmt(elk.nsd1.2))
  
        plot(elk.nsd1.1[["YL77 2004"]])
        plot(elk.nsd1.2[["YL77 2004"]])
     
        # & Repeat...
        plot(elk.nsd1.2[["YL15 2003"]])
  
        # Starting theta values is another good place to tinker
        pEst()
        pest.n3 <- pEst(s.t = 60)
        elk.nsd1.3 <- refine(elk.nsd1.2, pest.n3)
        all(fullmvmt(elk.nsd1.3))
  
        plot(elk.nsd1.3[["YL15 2003"]])
  

  ##----------------------------------------------------------------------------    
  #  III.	Inference		--------------
    
    #  A. Classification
  
        head(summary(elk.nsd1.3))
    		t.elk.nsd1.3 <- topmvmt(elk.nsd1.3)
    		
    		class(t.elk.nsd1.3)
    		class(t.elk.nsd1.3[[1]])
    		table(names(t.elk.nsd1.3))
        
        # visually check dispersers
          names(elk.nsd1.3)[which(names(t.elk.nsd1.3)%in%"disperser")]
          plot(elk.nsd1.3[["4049 2002"]])
          plot(elk.nsd1.3[["YL77 2004"]])  
  
    #  B. Description
    
      #  i.  Direct Estimates
  
          mvmt2df(t.elk.nsd1.3)
            
        # Comparing models for a single trajectory
         
          mvmt2df(elk.nsd1.3[["YL77 2004"]]@models)
  
        # Huge Tiny Mistake: comparing theta requiures "stdt"
        #  (for each trajectory, theta is measured from??)
            
          # t = 0 by trajectory
          t0 <- sapply(elk, function(x){as.POSIXlt(x$date[1])$yday + 1})
          hist(t0, breaks = seq(0, 7*14, by = 7), 
                 col = "grey", xlab ="Julian Date (binned by week)")
          elk

            # Refit NSD models to elk, specifying "stdt"
            stdt <- "3-1"  # formatted as "%m-%d" (see, e.g. ?strptime)
            elk.nsd2  <- mvmtClass(elk, stdt = stdt)
  
              # Check First Trajectory
              (e1d1 <- elk[[1]]$date[1])
              (e1m1 <- mvmt2df(elk.nsd1[[1]]@models["migrant"]))
              (e1m2 <- mvmt2df(elk.nsd2[[1]]@models["migrant"]))
              e1m2$migrant$theta - e1m1$migrant$theta
              difftime(e1d1, strptime("2002-03-01", "%F"))
              
      #  ii. Derived Estimates
  
        # Original Parameterization of Bunnefeld et al. 2011
          head(theta2(elk.nsd1))  # "timing of return migration"
          head(delta2(elk.nsd1))  # "distance of return migration"

        # Timing parameters to calendar  dates
          mvmt2dt(elk.nsd1)[19:22]
            
          elk.nsd1.mig <-topmvmt(elk.nsd1, omit = c("nomad", "mixmig",
                                                      "resident", "disperser"))
          elk.mig.p  <- mvmt2df(elk.nsd1.mig)$migrant
          elk.mig.p[elk.mig.p$rho==1, ]
          pEst()
  

  ##----------------------------------------------------------------------------    
  #  BONUS:	Combatting Misclassification		--------------

    # A. Model Families
  
      #  i.  rNSD (relative Net Squared Displacement)
		    elk.rloc <- findrloc(elk, stdt = stdt)
        elk.rnsd1 <- mvmtClass(elk, stdt = stdt, rloc = elk.rloc$rloc)    
  
        # Comparing 2 Difficult Trajectories from elk.nsd
        plot(elk.nsd1.3[[6]])
        elk.rloc[6, ]
        plot(elk.rnsd1[[6]])
        mvmt2df(elk.rnsd1[[6]]@models["migrant"])
  
      #  ii.  "elev"(ation)
  		  bhs.elev1 <- mvmtClass(bighorn, fam = "elev", stdt = "10-31")
  		  plot(mvmtClass(bighorn[1])[[1]])
        plot(mvmtClass(bighorn[1], rloc = findrloc(bighorn[1])$rloc)[[1]])
        plot(bhs.elev1[[1]])
        spatmig(bighorn[1], bhs.elev1[1])

        # Comparing Families
        bhs.elev1.1 <- refine(bhs.elev1, pEst(s.d=-600, u.d=0))
        spatmig(bighorn[3], mvmtClass(bighorn[3]))  # nsd
        spatmig(bighorn[3], bhs.elev1.1[3])         # elev
      
        par(mfrow=c(1,1))
  

    # B. Model Selection
      
      #  i.  parameter requirements
  
        # Set minimum migration distance
        elk.md <- topmvmt(elk.nsd1.3, mdelta = 100)
        table(names(t.elk.nsd1.3))
        table(names(elk.md))
  
        plot(elk.nsd1.3[[match("resident", names(elk.md))]])
  
        # Set minimum duration on second range
        elk.mr <- topmvmt(elk.nsd1.3, mrho = 21)
        table(names(elk.mr))
  
        plot(elk.nsd1.3[[match("resident", names(elk.mr))]])
  
      #  ii.  excluding models
        
        # Exclude "mixmig" models
        elk.nomm <- topmvmt(elk.nsd1.3, omit = "mixmig")
        table(names(elk.nomm))
     
        # Check "resident"
        plot(elk.nsd1.3[[which(names(elk.nomm)=="resident")]])
  
    # C. Excluding Points
      
      # Let's revisit the first plot we looked at for bighorn
      plot(mvmtClass(bighorn[3])[[1]])
  
      # "ecut": cut points from end of trajectory
      plot(mvmtClass(bighorn[3], ecut = "6-15")[[1]])
  
      # "scut" cut points from start of trajectory
      plot(bhs.elev1.1[[3]])
      pest.e2 <- pEst(s.d = -600)
      bhs3.scut.elev <- mvmtClass(bighorn[3], fam = "elev", scut="1-1", 
                                  p.est= pest.e2)
      plot(bhs3.scut.elev[[1]])
 
  
  ##----------------------------------------------------------------------------    
  #  Finding Help		--------------
  
    ?mvmtClass				                    #  Funcitons/Data Have Help Files
    library(help = migrateR)              #  Deets & Package Index
    vignette("migrateR")	                #  Background & Worked Examples
  
    # https://github.com/dbspitz/migrateR -- "issues"
    # spitz.derek@gmail.com               -- 
  
  