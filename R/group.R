group <- function(x, y, z=NULL, dataf, dist="t", conf.int=0.95, increment=1,
                  rolling=NULL, quarts=FALSE, cluster=FALSE ) {
  #Make "Increment" object equal to increment
  Increment <- increment

  #Create quartiles of a continuous variable and replace in output
  fncQuarts <- function(dataf, x) {
    x_labels <- c()
    for(i in 1:4) {
      x_labels[i] <- paste0("Q", i)
    }
    quints_ls <- quantile(dataf[, x])
    X <- cut(dataf[, x], breaks= quints_ls, labels=x_labels,
             right=TRUE, include.lowest=TRUE)
    return(list(X=X, Quartiles=quints_ls))
  }
  if(quarts==TRUE) {
    X.quartile <- fncQuarts(dataf=dataf, x=x)[[1]]
  }  else {
    X.quartile <- NULL
  }
  if(quarts==TRUE) {
    Quartiles <- fncQuarts(dataf=dataf, x=x)[[2]]
  }  else {
    Quartiles <- NULL
  }

  #Add quartile variable
  if(quarts==TRUE) {
    dataf <- data.frame(dataf[, -which(colnames(dataf) == x)], X.quartile)
    colnames(dataf)[which(colnames(dataf) =="X.quartile")] <- x
  }

  #Continuous outcomes
  tconf <- function(x, y, dataf, conf_lev) {
    #Aggregates outcome by factor
    all_m <- mean(dataf[, y], na.rm=T)
    all_sd <- sd(dataf[, y], na.rm=T)
    all_n <- length(na.omit(dataf[, y]))
    #By each X level
    agr_m <- aggregate(dataf[, y], list(dataf[, x]), FUN="mean", na.rm=T)
    agr_sd <- aggregate(dataf[, y], list(dataf[, x]), FUN="sd", na.rm=T)
    agr_n <- aggregate(dataf[ complete.cases(dataf[, y]) , y], list(dataf[ complete.cases(dataf[, y]) , x]), FUN="length")
    agr_df <- data.frame(x_lev=agr_m[, 1], agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
    #Calculates confidence intervals--Overall
    all_MOE <- qt((conf_lev/2)+.5, df=all_n - 1) * all_sd/sqrt(all_n)
    all_Lower <- all_m - all_MOE
    all_Upper <- all_m + all_MOE
    adf_all <- data.frame(cbind(PointEst=all_m, Lower=all_Lower, Upper=all_Upper))
    #Calculates confidence intervals--By Level
    MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
    Lower <- agr_df$agr_m - MOE
    Upper <- agr_df$agr_m + MOE
    adf_alpha <- data.frame(Group=agr_df$x_lev, PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper)
    rownames(adf_alpha) <- agr_df$x_lev
    #  alpha_o <- order(rownames(adf_alpha), decreasing = F)
    alpha_o <- order(agr_df$x_lev, decreasing = F)
    adf_alpha <- adf_alpha[alpha_o, ]
    adf_o <- order(adf_alpha[, "PointEst"], decreasing = F)
    adf_numeric <- adf_alpha[adf_o, ]
    return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric, adf_all= adf_all) )
  }
  ###################
  # Binary outcomes #
  ###################
  bconf <- function(x, y, dataf, conf_lev) {
    #Aggregates outcome by factor
    agr_sum <- aggregate(dataf[, y], list(dataf[, x]),  FUN="sum", na.rm=T)
    agr_n <- aggregate(dataf[, y], list(dataf[, x]), FUN="length")
    agr_df <- data.frame(x_lev=agr_sum[, 1], agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
    #Calculates confidence intervals
    adf_alpha <- binci(x=agr_df[,2], n=agr_df[,3], alpha=1 - conf_lev)
    adf_alpha <- data.frame(Group=agr_df$x_lev, adf_alpha)

    adf_all <- binci(x=sum(agr_df[,2], na.rm=TRUE), n=sum(agr_df[,3], na.rm=TRUE), alpha=1 - conf_lev)

    rownames(adf_alpha) <- agr_df$x_lev
    #  alpha_o <- order(rownames(adf_alpha), decreasing = F)
    alpha_o <- order(agr_df$x_lev, decreasing = F)
    adf_alpha <- adf_alpha[alpha_o, ]
    adf_o <- order(adf_alpha[, "PointEst"], decreasing = F)
    adf_numeric <- adf_alpha[adf_o, ]
    return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric, adf_all=adf_all) )
  }
  #################
  # Exact Poisson #
  #################
  pconf <- function(x, y, dataf, conf_lev) {
    #Aggregate outcomes for all
    all_sum <- sum(dataf[, y], na.rm=T)
    all_n <- length(na.omit(dataf[, y]))
    adf_all <- unlist(poisson.test(x=all_sum, T=all_n, conf.level= conf_lev)[c("estimate","conf.int")])
    adf_all <- data.frame(matrix(adf_all, ncol=3))
    colnames(adf_all) <- c("PointEst", "Lower", "Upper")
    #Aggregates outcome by factor
    agr_sum <- aggregate(dataf[, y], list(dataf[, x]), FUN="sum", na.rm=T)
    agr_n <- aggregate(dataf[, y], list(dataf[, x]), FUN="length")
    agr_df <- data.frame(x_lev=agr_sum[, 1], agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
    #Calculates confidence intervals
    adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
    for (i in 1:nrow(agr_df)) {
      adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,2], T=agr_df[i,3], conf.level= conf_lev)[c("estimate","conf.int")])
    }
    adf_alpha <- data.frame(Group=agr_df$x_lev, adf_alpha)
    colnames(adf_alpha) <- c("Group", "PointEst", "Lower", "Upper")
    rownames(adf_alpha) <- agr_df$x_lev
    #  alpha_o <- order(rownames(adf_alpha), decreasing = F)
    alpha_o <- order(agr_df$x_lev, decreasing = F)
    adf_alpha <- adf_alpha[alpha_o, ]
    adf_o <- order(adf_alpha[, "PointEst"], decreasing = F)
    adf_numeric <- adf_alpha[adf_o, ]
    return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric, adf_all=adf_all) )
  }

  conf <- function(x, y, dataf, conf_lev, dist) {
    switch(dist,
           "t" =  tconf(x, y, dataf, conf_lev),
           "b" =  bconf(x, y, dataf, conf_lev),
           "p" =  pconf(x, y, dataf, conf_lev)
    )
  }

  #######################################
  # Graphs for trajectories by time     #
  #######################################
  #Binomial
  fbconf <- function(x, xlev=NULL, y, z, dataf, conf_lev, Increment=1) {
    #Aggregates outcome by factor
    if( is.null(xlev)) {
      dataf <- dataf
    } else {
      dataf <- dataf[ dataf[, x] %in% xlev,  ]
    }
    #Calculates confidence intervals for single units or in increments
    if(Increment == 1) {
      agr_sum <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="length")
    } else {
      agr_sum <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="length")
    }
    agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
    agr_df <- cbind(agr_df, binci(x=agr_df[,3], n=agr_df[,4], alpha=1 - conf_lev))
    rownames(agr_df) <- 1:nrow(agr_df)
    return(agr_df)
  }

  #Continuous outcomes
  ftconf <- function(x, xlev=NULL, y, z, dataf, conf_lev, Increment=1) {
    Increment <- Increment
    #Aggregates outcome by factor
    if( is.null(xlev)) {
      dataf <- dataf
    } else {
      dataf <- dataf[ dataf[, x] %in% xlev,  ]
    }
    #Confidence interval data for increments
    if(Increment == 1) {
      agr_m <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="mean", na.rm=TRUE)
      agr_sd <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="sd", na.rm=TRUE)
      agr_n <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="length")
      agr_df <- data.frame(x_lev=agr_m[, 1], z_lev=as.integer(c(agr_m[, 2])) , agr_m=agr_m[, 3], agr_sd=agr_sd[, 3], agr_n=agr_n[, 3])
    } else {
      agr_m <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="mean", na.rm=TRUE)
      agr_sd <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="sd", na.rm=TRUE)
      agr_n <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="length")
      agr_df <- data.frame(x_lev=agr_m[, 1], z_lev=as.integer(c(agr_m[, 2])), agr_m=agr_m[, 3], agr_sd=agr_sd[, 3], agr_n=agr_n[, 3])
    }
    #Calculates confidence intervals
    MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
    Lower <- agr_df$agr_m - MOE
    Upper <- agr_df$agr_m + MOE
    adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper))
    agr_df <- cbind(agr_df, adf_alpha)
    return(agr_df )
  }

  #Exact Poisson
  fpconf <- function(x, xlev=NULL, y, z, dataf, conf_lev, Increment=1) {
    #Aggregates outcome by factor
    if( is.null(xlev)) {
      dataf <- dataf
    } else {
      dataf <- dataf[ dataf[, x] %in% xlev,  ]
    }
    #Confidence interval data for increments
    if(Increment == 1) {
      agr_sum <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="length")
      agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
    } else {
      agr_sum <- aggregate(dataf[, y] ~ dataf[, x]+ ceiling(dataf[, z]/Increment), FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(dataf[, y] ~ dataf[, x]+ ceiling(dataf[, z]/Increment), FUN="length")
      agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
    }
    #Calculates confidence intervals
    adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
    for (i in 1:nrow(agr_df)) {
      adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,3], T=agr_df[i,4], conf.level= conf_lev)[c("estimate","conf.int")])
    }
    adf_alpha <- data.frame(adf_alpha)
    colnames(adf_alpha) <- c("PointEst", "Lower", "Upper")
    agr_df <- cbind(agr_df, adf_alpha)
    return(agr_df=agr_df )
  }

  fconf <- function(x=xcivar, xlev=NULL, y=ycivar, z=zcivar, dataf, conf_lev=ciconf_lev,
                    Increment=1, dist) {
    switch(dist,
           "t" =  ftconf(x, xlev, y, z, dataf, conf_lev, Increment),
           "b" =  fbconf(x, xlev, y, z, dataf, conf_lev, Increment),
           "p" =  fpconf(x, xlev, y, z, dataf, conf_lev, Increment)
    )
  }
  # Rolling time #
  fncRollTime <- function(dataf, x, y, z, Increment, dist="t", conf_lev=.95) {
    #Get summary of values
    Time.Period.Length <- length(unique(dataf[, z]))
    Increment.Length <- Time.Period.Length - (Increment - 1)
    Time.Period.Values <- unique(dataf[, z])
    oTPV <- order(Time.Period.Values)
    Time.Period.Values <- Time.Period.Values[oTPV]
    #Get time periods for aggregating rates
    i <- 1
    Time.Periods <- list()
    while( i <= Increment.Length) {
      Time.Periods[[i]] <- Time.Period.Values[i:(i + (Increment -1))]
      i = i + 1
    }
    #Main title: Get first and last time points
    Time.Start.Label <- lapply(Time.Periods, `[[`, 1)
    Time.Stop.Label <- lapply(Time.Periods, `[[`, Increment)
    #Aggregate values
    AggrY <- list()
    for (i in 1:length(Time.Periods )) {
      switch(dist,
             "t"   = AggrY[[i]] <- tconf(x=x, y=y, dataf=dataf[dataf[, z] %in% Time.Periods[[i]], ],
                                         conf_lev=conf_lev),
             "b"   = AggrY[[i]] <- bconf(x=x, y=y, dataf=dataf[dataf[, z] %in% Time.Periods[[i]], ],
                                         conf_lev=conf_lev),
             "p"   = AggrY[[i]] <- pconf(x=x, y=y, dataf=dataf[dataf[, z] %in% Time.Periods[[i]], ],
                                         conf_lev=conf_lev)
      )
    }
    #Add in other values to each of the 3 lists
    for (i in 1:length(AggrY )) {
      AggrY[[i]][[1]]$Rolling <- Increment
      AggrY[[i]][[2]]$Rolling <- Increment
      AggrY[[i]][[3]]$Rolling <- Increment
    }
    for(j in 1:length(Time.Periods )) {
      for (i in 1:3) {
        AggrY[[j]][[i]]$Start <- Time.Start.Label[[j]]
        AggrY[[j]][[i]]$Stop <- Time.Stop.Label[[j]]
      }
    }
    roll_alpha <- do.call(rbind.data.frame, lapply(AggrY, `[[`, 1))
    roll_all <- do.call(rbind.data.frame, lapply(AggrY, `[[`, 3))
    return(list("Rolling"=roll_alpha, "Rolling.All"=roll_all))
  }
  # Run the function above #
  if(!is.null(rolling)) {
    roll_confint <- fncRollTime(dataf=dataf, x=x, y=y, z=z, Increment=rolling,
                                dist=dist, conf_lev=conf.int)
  } else {
    roll_confint <- NULL
  }

  #Group level clustering ICC, MOR, Design effects
  fncIccMorDe <- function(x, y, dataf, dist="t") {
    # one-way ANOVA
    anova_model <- aov(dataf[, y] ~ dataf[, x])
    # ANOVA table
    anova_results <- summary(anova_model)
    # Mean squares
    ms_bw <- anova_results[[1]]$"Mean Sq"[1]
    ms_wn <- anova_results[[1]]$"Mean Sq"[2]
    # number of observations per group
    k <- length(unique(dataf[, x]))
    #Average number per cluster
    kn <- mean(table(dataf[, x]))

    # Intraclass correlation #
    if(dist %in% c("t","p")) {
      ICC <- (ms_bw - ms_wn) / (ms_bw + (kn - 1) * ms_wn)
    }
    if(dist == "b") {
      ICC <- ms_bw/ (ms_bw + (3.14^2/3))
    }
    # Median Odds Ratio (MOR) #
    if(dist == "b") {
      MOR <- exp(sqrt(2 * ICC) * qnorm(0.75))
    } else {
      MOR <- NA
    }
    # Design effects (>= 2, don't ignore clustering) #
    DE <- 1 + (kn - 1) * ICC
    return(c("Intraclass.Correlation.Coefficient"=ICC, "Median.Odds.Ratio"=MOR,
             "Dessign.Effect"=DE))
  }
  #Run the function above
  if(cluster==TRUE) {
    cluster_res <- fncIccMorDe(x=x, y=y, dataf=dataf, dist=dist)
  } else {
    cluster_res <- NULL
  }

  main_confint <- conf(x=x, y=y, dataf=dataf, conf_lev=conf.int, dist=dist)
  if(!is.null(z)) {
    time_confint <- fconf(x=x, xlev=NULL, y=y, z=z, dataf, conf_lev=conf.int,
                          Increment=Increment, dist=dist)
  } else {
    time_confint <- NULL
  }

  return(list(Group.CI=main_confint, Time.CI=time_confint,
              Roll.CI=roll_confint, Clustering=cluster_res, Quartiles=Quartiles))
}


#group(x="program", y="los", dataf=hosprog)
#group(x="program", y="los", z="month", dataf=hosprog, dist="t", rolling=6)
#group(x="program", y="los", z="month", dataf=hosprog, dist="t", increment=3, rolling=6)
#group(x="risk", y="los", dataf=hosprog, quarts=TRUE, cluster=TRUE)
#group(x="risk", y="rdm30", dataf=hosprog, quarts=TRUE, dist="b", cluster=TRUE)
#group(x="risk", y="los2", dataf=hp2, quarts=TRUE, dist="p", cluster=TRUE)


## Citations ##
#ICC formula
#https://pmc.ncbi.nlm.nih.gov/articles/PMC4913118/#:~:text=Intraclass%20correlation%20coefficient%20was%20first,Table%202%20for%20their%20definitions).
#https://www.statmodel.com/discussion/messages/12/18.html

#Muthén, B. & Satorra, A. (1995). Complex sample data in structural equation modeling. Sociological Methodology, 25, 267-316.
#https://www.statmodel.com/download/SMMuthenSatorra1995.pdf
#https://www.statmodel.com/discussion/messages/12/18.html
#https://www.statmodel.com/discussion/messages/12/3076.html?1271109554

#Multilevel and Longitudinal Modeling Using Stata, Second Edition
#Authors:	Sophia Rabe-Hesketh and Anders Skrondal
#Publisher:	Stata Press
#Copyright:	2008
#ISBN-13:	978-1-59718-040-5
#Pages:	562; paperback

#A brief conceptual tutorial of multilevel analysis in social
#epidemiology: using measures of clustering in multilevel
#logistic regression to investigate contextual phenomena

#Juan Merlo, Basile Chaix, Henrik Ohlsson, Anders Beckman, Kristina Johnell, Per Hjerpe,
#L Ra˚stam, K Larsen
#J Epidemiol Community Health 2006;60:290–297. doi: 10.1136/jech.2004.029454
#J Epidemiol Community Health. 2006 Apr;60(4):290–297. doi: 10.1136/jech.2004.029454

